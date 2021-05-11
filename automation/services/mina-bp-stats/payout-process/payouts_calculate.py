import pandas as pd
from psycopg2 import extras
import os
import json
from google.cloud import storage
from payouts_config import BaseConfig
import psycopg2
from datetime import datetime, timezone

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

def get_gcs_client():
    credential_path = BaseConfig.CREDENTIAL_PATH
    os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = credential_path
    os.environ['GCS_API_KEY'] = BaseConfig.API_KEY
    return storage.Client()
    

def read_staking_json_list():
    storage_client = get_gcs_client()
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    staking_file_prefix = "staking-" 
    blobs = storage_client.list_blobs(bucket, start_offset=staking_file_prefix)
    # convert to string
    file_name_list_for_memory = list()
    for blob in blobs:
        file_name_list_for_memory.append(blob.name)
    return file_name_list_for_memory

def get_last_processed_epoch_from_audit():
    audit_query = '''select epoch_id from payout_audit_log where job_type='calculation' 
                    order by id desc limit 1'''
    last_epoch=0
    try:
        cursor = connection_payout.cursor()
        cursor.execute(audit_query)
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
    last_epoch = get_last_processed_epoch_from_audit()
    if last_epoch > 0:
        main(last_epoch+1)
    else:
        staking_ledger_avaialable = read_staking_json_list()
        for ledger in staking_ledger_avaialable:
            main(ledger.split('-')[1])


def read_staking_json_for_epoch(epoch_id):
    storage_client = get_gcs_client()
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    staking_file_prefix = "staking-" + str(epoch_id)
    blobs = storage_client.list_blobs(bucket, prefix=staking_file_prefix)
    # convert to string
    ledger_name=''
    modified_staking_df = pd.DataFrame()
    for blob in blobs:
        print(blob.name)
        ledger_name = blob.name
        json_data_string = blob.download_as_string()
        json_data_dict = json.loads(json_data_string)
        # print(json_data_dict)
        staking_df = pd.DataFrame(json_data_dict)
        modified_staking_df = staking_df[['pk', 'balance', 'delegate']]
        modified_staking_df['pk'] = modified_staking_df['pk'].astype(str)
        modified_staking_df['balance'] = modified_staking_df['balance'].astype(float)
        modified_staking_df['delegate'] = modified_staking_df['delegate'].astype(str)
        print(modified_staking_df.head().to_string())
    return modified_staking_df, ledger_name


def read_foundation_accounts():
    foundation_account_df = pd.read_csv('Mina_Foundation_Addresses.csv')
    print('foundation accounts dataframe ', foundation_account_df.shape)
    foundation_account_df.columns = ['pk']
    return foundation_account_df


def insert_data(df, page_size=100):
    tuples = [tuple(x) for x in df.to_numpy()]
    query = '''INSERT INTO  payout_summary (provider_pub_key, winner_pub_key,blocks,payout_amount, 
     payout_balance) VALUES (%s, %s, %s, %s, %s) 
      ON CONFLICT (provider_pub_key,winner_pub_key) 
      DO UPDATE SET payout_amount = payout_summary.payout_amount+EXCLUDED.payout_amount 
      '''
    result = 0
    try:
        cursor = connection_payout.cursor()
        extras.execute_batch(cursor, query, tuples, page_size)
        connection_payout.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: {0} ", format(error))
        connection_payout.rollback()
        cursor.close()
        result = -1
    finally:
        connection_payout.commit()
        cursor.close()
    return result


delegation_record_list = list()


def calculate_payout(modified_staking_df, foundation_bpk, epoch_id):
    filter_stake_df = modified_staking_df[modified_staking_df['pk'] == foundation_bpk]
    # calculate provider delegates accounts
    delegate_bpk = filter_stake_df['delegate'].values[0]
    delegation_df = modified_staking_df[modified_staking_df['delegate'] == delegate_bpk]
    # total stake
    total_stake = delegation_df['balance'].sum()
    total_stake = round(total_stake, 5)

    delegation_record_dict = dict()
    delegation_record_dict['provider_pub_key'] = filter_stake_df['pk'].values[0]
    delegation_record_dict['winner_pub_key'] = filter_stake_df['delegate'].values[0]

    # provider delegation
    provider_delegation = filter_stake_df['balance'].values[0]
    # delegation_record_dict['delegation_amount'] = provider_delegation

    # provider share
    provider_share = provider_delegation / total_stake

    # payout
    payout = (provider_share * 0.95) * BaseConfig.COINBASE

    # calculate blocks produced by delegate
    query = '''WITH RECURSIVE chain AS (
    (SELECT b.id, b.state_hash,parent_id, b.creator_id,b.height,b.global_slot_since_genesis/7140 AS epoch,b.staking_epoch_data_id FROM blocks b WHERE height = (select MAX(height) from blocks)
    ORDER BY timestamp ASC
    LIMIT 1)
    UNION ALL
    SELECT b.id, b.state_hash,b.parent_id, b.creator_id,b.height,b.global_slot_since_genesis/7140 AS epoch,b.staking_epoch_data_id FROM blocks b
    INNER JOIN chain
    ON b.id = chain.parent_id AND chain.id <> chain.parent_id
    ) SELECT count(*) as blocks_produced, pk.value as creator FROM chain c
    INNER JOIN public_keys pk
    ON pk.id = c.creator_id
    WHERE pk.value= %s
    and epoch = %s
    GROUP BY pk.value;
    '''

    cursor = connection_archive.cursor()
    cursor.execute(query, (delegate_bpk, str(epoch_id)))
    blocks_produced_list = cursor.fetchall()
    cursor.close()
    blocks_produced = 0
    if len(blocks_produced_list) > 0:
        blocks_produced = blocks_produced_list[0][0]
    delegation_record_dict['blocks'] = blocks_produced

    # calculate total payout
    total_payout = payout * blocks_produced
    total_payout = round(total_payout, 5)
    delegation_record_dict['payout_amount'] = total_payout
    delegation_record_dict['payout_balance'] = 0
    #delegation_record_dict['last_delegation_epoch'] = epoch_id
    delegation_record_list.append(delegation_record_dict)
    return delegation_record_list


def insert_into_audit_table(file_name):
    timestamp = datetime.now(timezone.utc)
    values = timestamp, file_name.split('-')[1], file_name, 'calculation'
    insert_audit_sql = """INSERT INTO payout_audit_log (updated_at, epoch_id, ledger_file_name,job_type) 
        values(%s, %s, %s, %s ) """
    try:
        cursor = connection_payout.cursor()
        cursor.execute(insert_audit_sql, values)
        connection_payout.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: {0} ", format(error))
        connection_payout.rollback()
        cursor.close()
    finally:
        cursor.close()
        connection_payout.commit()


def main(epoch_no):
    print("in main")
    #get staking json
    #modified_staking_df = pd.DataFrame()
    modified_staking_df, ledger_name = read_staking_json_for_epoch(epoch_no)
    print("modified_staking_df >>>>> \n" ,modified_staking_df)
    #TODO : add condition if no file/dataframe found
    # get foundation account details
    foundation_accounts_df = read_foundation_accounts()
    foundation_accounts_list = foundation_accounts_df['pk'].to_list()
    print('foundation accounts list', len(foundation_accounts_list))
    i = 0
    delegate_record_df = pd.DataFrame()
    for accounts in foundation_accounts_list:
        final_json_list = calculate_payout(modified_staking_df, accounts, epoch_no)
        delegate_record_df = pd.DataFrame(final_json_list)
        i = i + 1
    result = insert_data(delegate_record_df)
    if result == 0 :
        insert_into_audit_table(ledger_name)
    print('complete records for', i)

if __name__ == "__main__":
    initialize()
    
