import os
from datetime import datetime, timedelta, timezone
from time import time
import json
import numpy as np
import pandas as pd
from pandas.io.json import json_normalize
from config import BaseConfig
from google.cloud import storage
from collections import Counter
from sqlalchemy import create_engine
from sqlalchemy.dialects import postgresql
from download_batch_files import download_batch_into_memory

import psycopg2
import psycopg2.extras as extras

connection = psycopg2.connect(
    host=BaseConfig.POSTGRES_HOST,
    port=BaseConfig.POSTGRES_PORT,
    database=BaseConfig.POSTGRES_DB,
    user=BaseConfig.POSTGRES_USER,
    password=BaseConfig.POSTGRES_PASSWORD
)
#connection.autocommit = True

credential_path = "buoyant-purpose-307004-fdd8c8803a81.json"
os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = credential_path
os.environ['GCS_API_KEY'] = BaseConfig.API_KEY

start_time = time()


def Download_Files(start_offset, script_start_time, ten_min_add):
    storage_client = storage.Client()
    bucket = storage_client.get_bucket(BaseConfig.GCS_BUCKET_NAME)
    blobs = storage_client.list_blobs(bucket, start_offset=start_offset)
    file_name_list_for_memory = list()
    file_json_content_list = list()
    file_timestamps = list()
    
    for blob in blobs:
        if (blob.updated < ten_min_add) and (blob.updated > script_start_time):
            print(blob.name, "time: ", blob.updated, "size: ", blob.size)
            file_name_list_for_memory.append(blob.name)
            file_timestamps.append(blob.updated)
        elif blob.updated > ten_min_add:
            break
    print("file count for process", len(file_name_list_for_memory))
    
    if(len(file_name_list_for_memory)>0):
        file_contents = download_batch_into_memory(file_name_list_for_memory, bucket)
        file_name_list = list()
        for k, v in file_contents.items():
            file_name_list.append(k)
            file_json_content_list.append(json.loads(v))
        
        df = pd.json_normalize(file_json_content_list)
        df.drop(df.columns[[1,3,4,5,6,7,8,9,10,11,12,14]], axis=1, inplace=True)    
        df.insert(0, 'file_timestamps',file_timestamps)
        df.insert(0, 'file_name',file_name_list)
        print(df)
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
   
    query  = """INSERT INTO node_record_table ( block_producer_key,updated_at) 
            VALUES ( %s,  %s ) ON CONFLICT (block_producer_key) DO NOTHING """
    cursor = conn.cursor()
    try:
        
        extras.execute_batch(cursor, query, tuples, page_size)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
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
   
    query  = """INSERT INTO point_record_table ( file_name,blockchain_epoch, node_id, state_hash,blockchain_height,amount,bot_log_id, created_at) 
            VALUES ( %s,  %s, (SELECT id FROM node_record_table WHERE block_producer_key= %s), %s, %s, %s,  %s, %s )"""
    try:
        cursor = conn.cursor()
        extras.execute_batch(cursor, query, tuples, page_size)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return 1
    finally:
        conn.commit()
        cursor.close()
        return 0

def create_bot_log(conn, values):
    query  = """INSERT INTO bot_log_record_table(name_of_file,epoch_time,files_processed,batch_start_epoch,batch_end_epoch) values (%s,%s,
                    %s, %s, %s) RETURNING id """
    try:
        cursor = conn.cursor()
        cursor.execute(query, values)
        result = cursor.fetchone()
        conn.commit()
    
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return result[0]        


def update_scoreboard(conn):
    sql = """with score as ( select node_id,count(distinct bot_log_id) total from  point_record_table prt group by node_id )	
            update node_record_table nrt set score = total from score s where nrt.id=s.node_id"""
    try:
        cursor = conn.cursor()
        cursor.execut(query)
        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print("Error: %s" % error)
        conn.rollback()
        cursor.close()
        return -1
    finally:
        conn.commit()
        cursor.close()
        return 0

def GCS_main(read_file_interval):
    process_loop_count = 0
    bot_cursor = connection.cursor()
    bot_cursor.execute("SELECT batch_end_epoch FROM bot_log_record_table ORDER BY id DESC limit 1")
    result = bot_cursor.fetchone()
    batch_end_epoch = result[0]
    script_start_time = datetime.fromtimestamp((int(batch_end_epoch) / 1000), timezone.utc)
    script_end_time = datetime.now(timezone.utc)
    while script_start_time != script_end_time:
        
        # get 10 min time for fetching the files
        script_start_epoch = str(script_start_time.timestamp())
        
        ten_min_add = script_start_time + timedelta(minutes=read_file_interval)
        next_interval_epoch = str(ten_min_add.timestamp())

        # common str for offset
        script_start_time_final = str(script_start_time.date()) + '.' + str(script_start_time.timestamp())
        ten_min_add_final = str(ten_min_add.date()) + '.' + str(ten_min_add.timestamp())
        print(script_start_time_final)
        print(ten_min_add_final)
        common_str = os.path.commonprefix([script_start_epoch, next_interval_epoch])
        script_offset = str(script_start_time.date())  + '.' + common_str
        print('<<<<common str ', script_offset)

        # processing code logic
        master_df = Download_Files(script_offset, script_start_time, ten_min_add)
        all_file_count = master_df.shape[0]
        point_record_df = master_df
        
        try:
            # get the id of bot_log to insert in Point_record
            # last Epoch time & last filename
            if(not point_record_df.empty):
                last_file_name = master_df.iloc[-1]['file_name']
                last_filename_epoch_time = int(master_df.iloc[-1]['receivedAt'])

            else:
                last_file_name = ''
                last_filename_epoch_time = 0
            values = last_file_name, last_filename_epoch_time, all_file_count, script_start_time.timestamp(),ten_min_add.timestamp()
            bot_log_id = create_bot_log(connection, values)
            print( "========= bot_log_id : ",  bot_log_id)
            
            if(not point_record_df.empty):
                # min & max block height calculation
                #print(Counter(point_record_df['blockchain_height']))
                max_block_height = point_record_df['nodeData.blockHeight'].max()
                min_block_height = point_record_df['nodeData.blockHeight'].min()
                print('min block height', min_block_height, 'max block height', max_block_height)
                distinct_blockchain_height = point_record_df['nodeData.blockHeight'].nunique()
                if distinct_blockchain_height == 2:
                    height_filter_df = point_record_df.drop(
                        point_record_df[(point_record_df['nodeData.blockHeight'] == min_block_height)].index)
                elif distinct_blockchain_height > 2:
                    # filtered dataframe removed max and min height
                    height_filter_df = point_record_df.drop(
                        point_record_df[(point_record_df['nodeData.blockHeight'] == min_block_height) | (point_record_df[
                                                                                                        'nodeData.blockHeight'] == max_block_height)].index)
                
                most_common_state_hash = point_record_df['nodeData.block.stateHash'].value_counts().idxmax()
                print("most common state hash-mode", most_common_state_hash)
                point_record_df['amount'] = np.where(point_record_df['nodeData.block.stateHash'] == str(most_common_state_hash), 1, -1)
                
                final_point_record_df0 = point_record_df.loc[point_record_df['amount'] == 1]
                # create new dataframe for node record
                node_record_df = final_point_record_df0.filter(['blockProducerKey', 'amount'], axis=1)
                node_record_updated_df = node_record_df.groupby('blockProducerKey')['amount'].sum().reset_index()
                node_record_updated_df['updated_at'] = time()
                print(node_record_df)
                node_record_updated_df.rename(columns={'amount': 'score'}, inplace=True)
            
                # add node_id to point record dataframe
                
                final_point_record_df0.set_index('file_name', inplace=True)
                
                
                # data insertion to node_record_table
                table1_name = 'node_record_table'
                node_to_insert = node_record_updated_df[['blockProducerKey']]
                node_to_insert = node_to_insert.rename(columns={'blockProducerKey': 'block_producer_key'})
                node_to_insert['updated_at'] = int(time())

                execute_node_record_batch(connection, node_to_insert, 100)
                
                points_to_insert = final_point_record_df0[['receivedAt','blockProducerKey','nodeData.blockHeight','nodeData.block.stateHash','amount']]
                points_to_insert = points_to_insert.rename(columns={'receivedAt': 'blockchain_epoch','blockProducerKey':'block_producer_key','nodeData.blockHeight':'blockchain_height',
                        'nodeData.block.stateHash':'state_hash'})
                points_to_insert['created_at'] = int(time())       
                points_to_insert['bot_log_id'] = bot_log_id
                
                table_name = 'point_record_table'
                execute_point_record_batch(connection, points_to_insert)
                print('data in point records table is inserted')
                update_scoreboard(connection)
        except Exception as e:
            print(e)
            #transaction.rollback()
        finally:
            print("done")
            #connection.close()
        ## end if for count of files
        
        print('The last file information is added to DB')
        
        process_loop_count += 1
        print('Processed it', process_loop_count)
        # script_end_time = datetime.now(timezone.utc)
        # print("script end time updated", script_end_time)

        script_start_time = ten_min_add
        # script_start_time = last_file_timestamp
        print("script start time updated", script_start_time)
            

if __name__ == '__main__':
    time_interval = BaseConfig.read_file_interval
    GCS_main(time_interval)
