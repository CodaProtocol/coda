import pandas as pd
import psycopg2
from google.cloud import storage
import os
import json
from payouts_config import BaseConfig

connection_archive = psycopg2.connect(
    host=BaseConfig.POSTGRES_ARCHIVE_HOST,
    port=BaseConfig.POSTGRES_ARCHIVE_PORT,
    database=BaseConfig.POSTGRES_ARCHIVE_DB,
    user=BaseConfig.POSTGRES_ARCHIVE_USER,
    password=BaseConfig.POSTGRES_ARCHIVE_PASSWORD
)
connection_payout = psycopg2.connect(
    host=BaseConfig.POSTGRES_PAYOUT_HOST,
    port=BaseConfig.POSTGRES_PAYOUT_PORT,
    database=BaseConfig.POSTGRES_PAYOUT_DB,
    user=BaseConfig.POSTGRES_PAYOUT_USER,
    password=BaseConfig.POSTGRES_PAYOUT_PASSWORD
)


def read_delegation_record_table(epoch_no):
    curser = connection_payout.cursor()
    query = 'select * from payout_summary  where last_delegation_epoch = %s'
    curser.execute(query, str(epoch_no))
    delegation_record_list = curser.fetchall()
    delegation_record_df = pd.DataFrame(delegation_record_list,
                                        columns=['provider_pub_key', 'winner_pub_key', 'blocks', 'payout_amount',
                                                 'payout_balance', 'last_delegation_epoch'])
    curser.close()
    #print(delegation_record_df.head().to_string())
    return delegation_record_df


def read_staking_json(epoch_no):
    credential_path = "mina-mainnet-303900-45050a0ba37b.json"
    os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = credential_path
    os.environ['GCS_API_KEY'] = BaseConfig.API_KEY
    # create storage client
    storage_client = storage.Client()
    # get bucket with name
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    # get bucket data as blob')
    staking_file_prefix = "staking-" + str(epoch_no)
    blobs = storage_client.list_blobs(bucket, prefix=staking_file_prefix)
    # convert to string
    for blob in blobs:
        print(blob.name)
        json_data_string = blob.download_as_string()
        json_data_dict = json.loads(json_data_string)
        # print(json_data_dict)
        staking_df = pd.DataFrame(json_data_dict)
        modified_staking_df = staking_df[['pk', 'balance', 'delegate']]
        modified_staking_df['pk'] = modified_staking_df['pk'].astype(str)
        modified_staking_df['balance'] = modified_staking_df['balance'].astype(float)
        modified_staking_df['delegate'] = modified_staking_df['delegate'].astype(str)
        print(modified_staking_df.head().to_string())
    return modified_staking_df


def get_record_for_validation(epoch_no):
    cursor = connection_archive.cursor()
    genesis_slot_start_range = (epoch_no-1) * 7014 + 3500
    genesis_slot_end_range = epoch_no * 7140 + 3500
    print(genesis_slot_start_range, genesis_slot_end_range)

    query = '''WITH RECURSIVE chain AS (
    (SELECT b.id, b.state_hash,parent_id, b.creator_id,b.height,b.global_slot_since_genesis,b.global_slot_since_genesis/7140 as epoch,b.staking_epoch_data_id
    FROM blocks b WHERE height = (select MAX(height) from blocks)
    ORDER BY timestamp ASC
    LIMIT 1)
    UNION ALL
    SELECT b.id, b.state_hash,b.parent_id, b.creator_id,b.height,b.global_slot_since_genesis,b.global_slot_since_genesis/7140 as epoch,b.staking_epoch_data_id
    FROM blocks b
    INNER JOIN chain ON b.id = chain.parent_id AND chain.id <> chain.parent_id
    ) SELECT  sum(amount)/power(10,9) as total_pay, pk.value as creator ,epoch
    FROM chain c INNER JOIN blocks_user_commands AS buc on c.id = buc.block_id
    inner join (SELECT * FROM user_commands where type='payment' ) AS uc on
     uc.id = buc.user_command_id and status <>'failed'
    INNER JOIN public_keys as PK ON PK.id = uc.receiver_id
    where global_slot_since_genesis BETWEEN %s and %s 
    GROUP BY pk.value, epoch'''

    cursor.execute(query, (genesis_slot_start_range, genesis_slot_end_range))
    validation_record_list = cursor.fetchall()
    validation_record_df = pd.DataFrame(validation_record_list,
                                        columns=['total_pay', 'provider_pub_key', 'epoch'])
    cursor.close()
    #print(validation_record_df.head().to_string())
    return validation_record_df


def main(epoch_no):
    print("###### in main for epoch: ", epoch_no)
    delegation_record_df = read_delegation_record_table(epoch_no=epoch_no)
    validation_record_df = get_record_for_validation(epoch_no=epoch_no)
    staking_df = read_staking_json(epoch_no=epoch_no)
    print('######## before for loop: \n',delegation_record_df)
    for row in delegation_record_df.itertuples():
        pub_key = getattr(row, "provider_pub_key")
        payout_amount = getattr(row, "payout_amount")
        filter_validation_record_df = validation_record_df.loc[validation_record_df['provider_pub_key'] == pub_key]
        print('######## before if: \n',filter_validation_record_df)
        if not filter_validation_record_df.empty:
            print('filter validation df', len(filter_validation_record_df))
            print(filter_validation_record_df.to_string())
            total_pay_received = filter_validation_record_df.iloc[0]['total_pay']
            # read winner account from staking json
            filter_staking_df = staking_df.loc[staking_df['pk'] == pub_key, 'delegate']
            print('filter staking df', len(filter_staking_df))
            print(filter_staking_df.to_string())
            winner_pub_key = filter_staking_df.iloc[0]
            print('######## winner pub_key', winner_pub_key)
            # update record in payout summary
            query = ''' UPDATE payout_summary SET payout_amount = 0, payout_balance = payout_amount-%s,
                last_delegation_epoch = %s
                WHERE provider_pub_key = %s and winner_pub_key = %s
                '''
            try:
                cursor = connection_payout.cursor()
                cursor.execute(query, (total_pay_received, epoch_no, pub_key, winner_pub_key))
            except (Exception, psycopg2.DatabaseError) as error:
                print("Error: {0} ", format(error))
                connection_payout.rollback()
                cursor.close()
                result = -1
            finally:
                print('######## updated successfully')
                connection_payout.commit()
                cursor.close()    
            print('update into table')

def get_last_processed_epoch_from_audit(job_type):
    audit_query = '''select epoch_id from payout_audit_log where job_type=%s 
                    order by id desc limit 1'''
    last_epoch=0
    values = job_type,
    try:
        cursor = connection_payout.cursor()
        cursor.execute(audit_query, values)
        if cursor.rowcount > 0:
            data_count = cursor.fetchall()
            last_epoch = float(data_count[-1][-1])
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: {0} ", format(error))
        cursor.close()
        return -1
    finally:
        cursor.close()
    return last_epoch

# this will check audit log table, and will determine last processed epoch
# if no entries found, default to first epoch
def initialize():
    last_epoch = get_last_processed_epoch_from_audit('validation')
    print(last_epoch)
    if last_epoch > 0:
        print(" validation Audit found for")
        main(last_epoch+1)
    else:
        last_epoch = get_last_processed_epoch_from_audit('calculation')
        print(" calculation Audit found for", last_epoch)
        count =1
        while count <= last_epoch:
            print(count)
            main(count)
            count = count+1
    print("initialize complete ")


if __name__ == "__main__":
   initialize()
