import pandas as pd
from psycopg2 import extras
import os
import json
from google.cloud import storage
from configCalculatePayout import BaseConfig
import psycopg2

connection = psycopg2.connect(
    host=BaseConfig.POSTGRES_HOST,
    port=BaseConfig.POSTGRES_PORT,
    database=BaseConfig.POSTGRES_DB,
    user=BaseConfig.POSTGRES_USER,
    password=BaseConfig.POSTGRES_PASSWORD
)


def read_staking_json(epoch_id):
    credential_path = "mina-mainnet-303900-45050a0ba37b.json"
    os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = credential_path
    os.environ['GCS_API_KEY'] = BaseConfig.API_KEY
    # create storage client
    storage_client = storage.Client()
    # get bucket with name
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    # get bucket data as blob')
    staking_file_prefix = "staking-" + str(epoch_id)
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


def read_foundation_accounts():
    foundation_account_df = pd.read_csv('Mina_Foundation_Addresses.csv')
    print('foundation accounts dataframe ', foundation_account_df.shape)
    foundation_account_df.columns = ['pk']
    return foundation_account_df


def insert_data(df, page_size=100):
    tuples = [tuple(x) for x in df.to_numpy()]
    query = '''INSERT INTO  payout_summary (provider_pub_key, winner_pub_key,blocks,payout_amount, 
     payout_balance, last_delegation_epoch) VALUES (%s, %s , %s, %s, %s, %s) 
      ON CONFLICT (provider_pub_key,winner_pub_key) 
      DO 
      UPDATE SET payout_amount = EXCLUDED.payout_amount , last_delegation_epoch = EXCLUDED.last_delegation_epoch
      '''
    try:
        cursor = connection.cursor()
        extras.execute_batch(cursor, query, tuples, page_size)
        connection.commit()
        print('Done into table')
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: {0} ", format(error))
        connection.rollback()
        cursor.close()
        return 1
    finally:
        connection.commit()
        cursor.close()
        return 0


delegation_record_list = list()


def calculate_payout(modified_staking_df, foundation_bpk, epoch_id):
    filter_stake_df = modified_staking_df[modified_staking_df['pk'] == foundation_bpk]
    print(filter_stake_df.to_string())
    # calculate provider delegates accounts
    delegate_bpk = filter_stake_df['delegate'].values[0]
    print(delegate_bpk)
    delegation_df = modified_staking_df[modified_staking_df['delegate'] == delegate_bpk]
    print('dataframe for account who delegate', delegation_df.shape)
    print('type of columns', delegation_df.dtypes)

    # total stake
    total_stake = delegation_df['balance'].sum()
    total_stake = round(total_stake, 5)
    print('calculate total_stake ', total_stake)

    delegation_record_dict = dict()
    delegation_record_dict['provider_pub_key'] = filter_stake_df['pk'].values[0]
    delegation_record_dict['winner_pub_key'] = filter_stake_df['delegate'].values[0]

    # provider delegation
    provider_delegation = filter_stake_df['balance'].values[0]
    print('provider delegation for ', provider_delegation)
    # delegation_record_dict['delegation_amount'] = provider_delegation

    # provider share
    provider_share = provider_delegation / total_stake
    print('calculate provider share', provider_share)

    # payout
    payout = (provider_share * 0.95) * BaseConfig.COINBASE
    print('calculate payout', payout)

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

    cursor = connection.cursor()
    cursor.execute(query, (delegate_bpk, str(epoch_id)))
    blocks_produced_list = cursor.fetchall()
    blocks_produced = 0
    if len(blocks_produced_list) > 0:
        blocks_produced = blocks_produced_list[0][0]
    delegation_record_dict['blocks'] = blocks_produced
    print('blocks produced by delegate', blocks_produced)

    # calculate total payout
    total_payout = payout * blocks_produced
    total_payout = round(total_payout, 5)
    print('total payout', total_payout)
    delegation_record_dict['payout_amount'] = total_payout
    delegation_record_dict['payout_balance'] = 0
    delegation_record_dict['last_delegation_epoch'] = epoch_id
    delegation_record_list.append(delegation_record_dict)
    return delegation_record_list


def main(epoch_no):
    # get staking json
    modified_staking_df = read_staking_json(epoch_no)
    # get foundation account details
    foundation_accounts_df = read_foundation_accounts()
    foundation_accounts_list = foundation_accounts_df['pk'].to_list()
    print('foundation accounts list', len(foundation_accounts_list))
    i = 0
    delegate_record_df = pd.DataFrame()
    for accounts in foundation_accounts_list:
        print('loop ', i)
        final_json_list = calculate_payout(modified_staking_df, accounts, epoch_no)
        delegate_record_df = pd.DataFrame(final_json_list)
        i = i + 1
    insert_data(delegate_record_df)
    print('complete records for', i)


if __name__ == "__main__":
    main(epoch_no=2)
