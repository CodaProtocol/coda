--drop database minanet_payout;
create database minanet_payout;

--delete from payout_summary;
--delete from payout_audit_log;

--drop table payout_audit_log

CREATE TYPE job_execution_type AS ENUM (
	'calculation',
	'validation');

CREATE TABLE payout_summary
(
    provider_pub_key character varying(280) COLLATE pg_catalog."default" NOT NULL,
    winner_pub_key character varying(280) COLLATE pg_catalog."default" NOT NULL,
    blocks integer,
    payout_amount double precision,
    payout_balance double precision,
    last_delegation_epoch bigint,
    last_slot_validated bigint,
    CONSTRAINT payout_summary_pkey PRIMARY KEY (provider_pub_key, winner_pub_key)
);


create table payout_audit_log(
	id serial not null,
	updated_at timestamp NOT NULL,
	epoch_id bigint NULL,
	ledger_file_name char(100) NULL,
	job_type job_execution_type NOT null,
    CONSTRAINT payout_audit_log_pkey PRIMARY KEY (id)
);