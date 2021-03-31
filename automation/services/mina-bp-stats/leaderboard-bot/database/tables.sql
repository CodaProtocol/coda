drop table if exists point_record_table;

drop table if exists node_record_table;

drop table if exists bot_log_record_table;

create table node_record_table(ID SERIAL primary key, block_producer_key character varying unique, score int, updated_at timestamp);

create table bot_log_record_table(ID SERIAL primary key, name_of_file character varying, epoch_time bigint, files_processed int, file_timestamps timestamp, batch_start_epoch bigint, batch_end_epoch bigint);

create table point_record_table(ID SERIAL primary key, file_name character varying, blockchain_epoch bigint, blockchain_height bigint, state_hash character varying, 
				created_at timestamp, amount int, node_id int, bot_log_id int,
				CONSTRAINT fk_node_record FOREIGN KEY(node_id)  REFERENCES node_record_table(id),
				CONSTRAINT fk_bot_log FOREIGN KEY(bot_log_id)  REFERENCES bot_log_record_table(id)
				);


insert into bot_log_record_table(name_of_file,epoch_time,files_processed,file_timestamps,batch_start_epoch,batch_end_epoch) 
	values('2021-03-17.1615939200000.1894.json',1615939200000,0,'2021-03-10 23:08:47', 1615939200000,1615939200000);