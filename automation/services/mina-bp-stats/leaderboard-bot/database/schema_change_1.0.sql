alter table node_record_table
	add score_percent numeric(6,2),
	add discord_id varchar ,
	add application_status boolean default false,
	add block_producer_email varchar;

alter table point_record_table
	add file_timestamps timestamp;