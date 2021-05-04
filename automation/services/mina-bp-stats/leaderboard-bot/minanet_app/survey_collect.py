import os
from datetime import datetime, timedelta, timezone
from time import time
import json
import numpy as np
from logger_util import logger
from config import BaseConfig
from google.cloud import storage
from download_batch_files import download_batch_into_memory
import requests
import io
import psycopg2
import psycopg2.extras as extras
import gspread
import pandas as pd
from oauth2client.service_account import ServiceAccountCredentials

connection = psycopg2.connect(
    host=BaseConfig.POSTGRES_HOST,
    port=BaseConfig.POSTGRES_PORT,
    database=BaseConfig.POSTGRES_DB,
    user=BaseConfig.POSTGRES_USER,
    password=BaseConfig.POSTGRES_PASSWORD
)

start_time = time()


def Download_Files(start_offset, script_start_time, ten_min_add):
    storage_client = storage.Client.from_service_account_json(BaseConfig.CREDENTIAL_PATH)
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    blobs = storage_client.list_blobs(bucket, start_offset=start_offset)
    file_name_list_for_memory = list()
    file_json_content_list = list()
    file_timestamps = list()

    for blob in blobs:
        if (blob.updated < ten_min_add) and (blob.updated > script_start_time):
            msg = 'name : {0} time: {1} size: {2}'.format(blob.name, blob.updated, blob.size)
            logger.info(msg)
            file_name_list_for_memory.append(blob.name)
            file_timestamps.append(blob.updated)
        elif blob.updated > ten_min_add:
            break
    file_count = len(file_name_list_for_memory)
    logger.info('file count for process : {0}'.format(file_count))

    if len(file_name_list_for_memory) > 0:
        file_contents = download_batch_into_memory(file_name_list_for_memory, bucket)
        file_name_list = list()
        for k, v in file_contents.items():
            file_name_list.append(k)
            file_json_content_list.append(json.loads(v))

        df = pd.json_normalize(file_json_content_list)
        df.drop(df.columns[[1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14]], axis=1, inplace=True)
        df.insert(0, 'file_timestamps', file_timestamps)
        df.insert(0, 'file_name', file_name_list)
    else:
        df = pd.DataFrame()
    return df


def execute_node_record_batch(conn, df, page_size=100):
    """
    Using psycopg2.extras.execute_batch() to insert node records dataframe
    Make sure datafram has exact following columns
    block_producer_key, updated_at
    """
    tuples = [tuple(x) for x in df.to_numpy()]
    query = """INSERT INTO node_record_table ( block_producer_key,updated_at) 
            VALUES ( %s,  %s ) ON CONFLICT (block_producer_key) DO NOTHING """
    cursor = conn.cursor()
    try:

        extras.execute_batch(cursor, query, tuples, page_size)
        conn.commit()
        logger.info('insert into node record table')
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0}", format(error))
        conn.rollback()
        cursor.close()
        return 1
    finally:
        conn.commit()
        cursor.close()
        return 0


def execute_point_record_batch(conn, df, page_size=100):
    """
    Using psycopg2.extras.execute_batch() to insert point records dataframe
    Make sure datafram has exact following columns in sequence
    file_name,blockchain_epoch, block_producer_key, state_hash,blockchain_height,amount,bot_log_id, created_at
    """
    tuples = [tuple(x) for x in df.to_numpy()]
    logger.info('Tuples {0}'.format(tuples[0]))
    query = """INSERT INTO point_record_table ( file_name,blockchain_epoch, node_id, state_hash,blockchain_height,amount,created_at,bot_log_id) 
            VALUES ( %s,  %s, (SELECT id FROM node_record_table WHERE block_producer_key= %s), %s, %s, %s,  %s, %s )"""
    try:
        cursor = conn.cursor()
        extras.execute_batch(cursor, query, tuples, page_size)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0} ", format(error))
        conn.rollback()
        cursor.close()
        return 1
    finally:
        conn.commit()
        cursor.close()
        return 0


def create_bot_log(conn, values):
    query = """INSERT INTO bot_log_record_table(name_of_file,epoch_time,files_processed,file_timestamps,
    batch_start_epoch,batch_end_epoch) values (%s,%s, %s, %s, %s, %s) RETURNING id """
    try:
        cursor = conn.cursor()
        cursor.execute(query, values)
        result = cursor.fetchone()
        conn.commit()
        logger.info("insert into bot log record table")
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info('Error: {0}', format(error))
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return result[0]


def connect_to_spreadsheet():
    os.environ["PYTHONIOENCODING"] = "utf-8"
    creds = ServiceAccountCredentials.from_json_keyfile_name(BaseConfig.SPREADSHEET_JSON, BaseConfig.SPREADSHEET_SCOPE)
    client = gspread.authorize(creds)
    sheet = client.open(BaseConfig.SPREADSHEET_NAME)
    sheet_instance = sheet.get_worksheet(0)
    records_data = sheet_instance.get_all_records()
    table_data = pd.DataFrame(records_data)
    return table_data


def update_email_discord_status(conn, page_size=100):
    # 4 - block_producer_key,  3 - block_producer_email , # 2 - discord_id
    spread_df = connect_to_spreadsheet()
    spread_df = spread_df.iloc[:, [2, 3, 4]]
    tuples = [tuple(x) for x in spread_df.to_numpy()]

    try:
        cursor = conn.cursor()
        sql = """update node_record_table set application_status = true, discord_id =%s, block_producer_email =%s
             where block_producer_key= %s """
        cursor = conn.cursor()
        extras.execute_batch(cursor, sql, tuples, page_size)
        conn.commit()
        logger.info('update email discord status')
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info('Error:{0}', format(error))
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return 0


def get_provider_accounts():
    # read csv
    mina_foundation_df = pd.read_csv(BaseConfig.PROVIDER_ACCOUNT_PUB_KEYS_FILE)
    mina_foundation_df.columns = ['block_producer_key']
    return mina_foundation_df


def update_scoreboard(conn):
    sql = """with score as ( select node_id,count(distinct bot_log_id) total from  point_record_table  prt where
     created_at > current_date - interval '%s' day group by 
    node_id ) update node_record_table nrt set score = total from score s where nrt.id=s.node_id """
    try:
        cursor = conn.cursor()
        cursor.execute(sql, (BaseConfig.UPTIME_DAYS_FOR_SCORE,))
        conn.commit()
        logger.info('update score board')
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info('Error:{0}', format(error))
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return 0


def update_score_percent(conn):
    # to calculate percentage, first find the number of survey_intervals.
    # the number of rows in blot_log represent number of time survey performed.
    count_query = """select count(1) +1  from bot_log_record_table where file_timestamps > current_date - interval 
    '%s' day """

    sql = """update node_record_table set score_percent = (score / %s ) * %s """
    try:
        cursor = conn.cursor()
        cursor.execute(count_query, (BaseConfig.UPTIME_DAYS_FOR_SCORE,))
        data_count = cursor.fetchall()
        data_count = float(data_count[-1][-1])
        percent = 100
        cursor.execute(sql, (data_count, percent))
        conn.commit()
        logger.info('update score percent')
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info('Error:{0}', format(error))
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return 0


def GCS_main(read_file_interval):
    update_email_discord_status(connection)
    process_loop_count = 0
    bot_cursor = connection.cursor()
    bot_cursor.execute("SELECT batch_end_epoch FROM bot_log_record_table ORDER BY id DESC limit 1")
    result = bot_cursor.fetchone()
    batch_end_epoch = result[0]
    script_start_time = datetime.fromtimestamp(batch_end_epoch, timezone.utc)
    script_end_time = datetime.now(timezone.utc)
    logger.info('script start time {0}'.format(script_start_time))
    logger.info('script end time {0}'.format(script_end_time))
    # read the mina foundation accounts
    foundation_df = get_provider_accounts()
    while script_start_time != script_end_time:
        # get 10 min time for fetching the files
        script_start_epoch = str(script_start_time.timestamp())

        ten_min_add = script_start_time + timedelta(minutes=read_file_interval)
        next_interval_epoch = str(ten_min_add.timestamp())

        # common str for offset
        script_start_time_final = str(script_start_time.date()) + '.' + str(script_start_time.timestamp())
        ten_min_add_final = str(ten_min_add.date()) + '.' + str(ten_min_add.timestamp())
        logger.info(script_start_time_final)
        logger.info(ten_min_add_final)

        # change format for comparison
        script_end_time_var = datetime.strftime(script_end_time, '%Y-%m-%d %H:%M:%S')
        ten_min_add_time_var = datetime.strftime(ten_min_add, '%Y-%m-%d %H:%M:%S')
        if ten_min_add_time_var > script_end_time_var:
            logger.info('all files are processed till date')
            break
        else:
            logger.info('files are remaining for processing')
            common_str = os.path.commonprefix([script_start_epoch, next_interval_epoch])
            script_offset = str(script_start_time.date()) + '.' + common_str
            logger.info('common str{} '.format(script_offset))

            # processing code logic
            master_df = Download_Files(script_offset, script_start_time, ten_min_add)
            all_file_count = master_df.shape[0]
            # remove the Mina foundation account from the master_df

            master_df = master_df[~master_df['blockProducerKey'].isin(foundation_df['block_producer_key'])]

            point_record_df = master_df

            try:
                # get the id of bot_log to insert in Point_record
                # last Epoch time & last filename
                if not point_record_df.empty:
                    last_file_name = master_df.iloc[-1]['file_name']
                    last_filename_epoch_time = int(master_df.iloc[-1]['receivedAt'])
                    file_timestamp = master_df.iloc[-1]['file_timestamps']
                else:
                    last_file_name = ''
                    last_filename_epoch_time = 0
                    file_timestamp = 0
                values = last_file_name, last_filename_epoch_time, all_file_count, file_timestamp, script_start_time.timestamp(), ten_min_add.timestamp()
                bot_log_id = create_bot_log(connection, values)
                if not point_record_df.empty:
                    logger.info('point record is not empty')
                    # min & max block height calculation
                    # logger.info(Counter(point_record_df['blockchain_height']))
                    max_block_height = point_record_df['nodeData.blockHeight'].max()
                    min_block_height = point_record_df['nodeData.blockHeight'].min()
                    logger.info(
                        'min block height{0} and max block height{1} '.format(min_block_height, max_block_height))
                    distinct_blockchain_height = point_record_df['nodeData.blockHeight'].nunique()
                    if distinct_blockchain_height == 2:
                        height_filter_df = point_record_df.drop(
                            point_record_df[(point_record_df['nodeData.blockHeight'] == min_block_height)].index)
                    elif distinct_blockchain_height > 2:
                        # filtered dataframe removed max and min height
                        height_filter_df = point_record_df.drop(
                            point_record_df[
                                (point_record_df['nodeData.blockHeight'] == min_block_height) | (point_record_df[
                                                                                                     'nodeData'
                                                                                                     '.blockHeight']
                                                                                                 ==
                                                                                                 max_block_height)].index)

                    most_common_state_hash = point_record_df['nodeData.block.stateHash'].value_counts().idxmax()

                    point_record_df['amount'] = np.where(
                        point_record_df['nodeData.block.stateHash'] == str(most_common_state_hash), 1, -1)

                    final_point_record_df0 = point_record_df.loc[point_record_df['amount'] == 1]
                    # create new dataframe for node record
                    node_record_df = final_point_record_df0.filter(['blockProducerKey', 'amount'], axis=1)
                    node_record_updated_df = node_record_df.groupby('blockProducerKey')['amount'].sum().reset_index()

                    node_record_updated_df.rename(columns={'amount': 'score'}, inplace=True)

                    # data insertion to node_record_table
                    node_to_insert = node_record_updated_df[['blockProducerKey']]
                    node_to_insert = node_to_insert.rename(columns={'blockProducerKey': 'block_producer_key'})
                    node_to_insert['updated_at'] = datetime.now(timezone.utc)

                    execute_node_record_batch(connection, node_to_insert, 100)

                    points_to_insert = final_point_record_df0
                    points_to_insert = points_to_insert.rename(
                        columns={'receivedAt': 'blockchain_epoch', 'blockProducerKey': 'block_producer_key',
                                 'nodeData.blockHeight': 'blockchain_height',
                                 'nodeData.block.stateHash': 'state_hash'})
                    points_to_insert['created_at'] = datetime.now(timezone.utc)
                    points_to_insert['bot_log_id'] = bot_log_id
                    points_to_insert.drop('file_timestamps', inplace=True, axis=1)
                    execute_point_record_batch(connection, points_to_insert)
                    logger.info('data in point records table is inserted')
                    update_scoreboard(connection)
                    update_score_percent(connection)
            except Exception as e:
                logger.info(e)
            finally:
                logger.info('done')

            logger.info('The last file information is added to DB')

            process_loop_count += 1
            logger.info('Processed it loop count : {0}'.format(process_loop_count))

            script_start_time = ten_min_add
            logger.info('script start time updated:  {0}'.format(script_start_time))


if __name__ == '__main__':
    time_interval = BaseConfig.SURVEY_INTERVAL_MINUTES
    GCS_main(time_interval)
