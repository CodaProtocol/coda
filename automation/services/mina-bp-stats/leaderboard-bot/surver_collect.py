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

db_string = BaseConfig.SQLALCHEMY_DATABASE_URI
db = create_engine(db_string,pool_size=20, max_overflow=0)
credential_path = "mina-mainnet-303900-b9056c625e58.json"
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


def GCS_main(read_file_interval):
    process_loop_count = 0
    
    batch_end_epoch = db.execute("SELECT batch_end_epoch FROM bot_log_record_table WHERE file_processed=0")
    script_start_time = datetime.fromtimestamp((int(batch_end_epoch.first()[0]) / 1000), timezone.utc)
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
        conn = db.connect()
        transaction = conn.begin()
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
            result = db.execute("""INSERT INTO bot_log_record_table(name_of_file,epoch_time,file_processed,batch_start_epoch,batch_end_epoch) values (%s,%s,
                    %s, %s, %s) RETURNING id """,  values)
            bot_log_id = result.fetchone()[0]
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
                node_record_updated_df['updated_at'] = int(time())
                node_record_updated_df.rename(columns={'amount': 'score'}, inplace=True)
            
                # add node_id to point record dataframe
                
                final_point_record_df0.set_index('file_name', inplace=True)
                
                
                # data insertion to node_record_table
                table1_name = 'node_record_table'
                node_to_insert = node_record_updated_df[['blockProducerKey']]
                node_to_insert = node_to_insert.rename(columns={'blockProducerKey': 'block_producer_key'})
                node_to_insert['updated_at'] = int(time())
            
                #upsert(db,'public','node_record_table', node_to_insert)
                print("====================  Node Data to Insert")
                print(node_to_insert)
                # data insertion to point_record_table
                try:
                    node_to_insert.to_sql(table1_name, db, if_exists='append', index=False)
                except Exception:
                    pass      
                
                
                points_to_insert = final_point_record_df0[['receivedAt','blockProducerKey','nodeData.blockHeight','nodeData.block.stateHash','amount']]
                points_to_insert = points_to_insert.rename(columns={'receivedAt': 'blockchain_epoch','blockProducerKey':'block_producer_key','nodeData.blockHeight':'blockchain_height',
                        'nodeData.block.stateHash':'state_hash'})
                points_to_insert['created_at'] = int(time())       
                points_to_insert['bot_log_id'] = bot_log_id
                
                table_name = 'point_record_table'
                points_to_insert.to_sql(table_name, db, if_exists='append')
                transaction.commit()
                print('data in point records table is inserted')
        except Exception:
            transaction.rollback()
        finally:
            conn.close()
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
