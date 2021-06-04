from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
import base64
import pandas.io.sql as sqlio
from config import BaseConfig
import survey_collect
from logger_util import logger

LEADERBOARD_RECORDS_CSV = 'leaderboard_records.csv'
DEPARTING_NODE_RECORD_CSV = 'departing_node_record.csv'

conn = survey_collect.connection
query = """ SELECT block_producer_key , score FROM (select *
            from node_record_table
            where updated_at > current_timestamp - interval '60 day') AS records ORDER BY score DESC LIMIT 120;"""
dat = sqlio.read_sql_query(query, conn)
dat.to_csv(LEADERBOARD_RECORDS_CSV, index=False)
logger.info('leaderboard_records csv file generated')

query2 = """with lastboard as (
         select node_id,count(distinct bot_log_id) total
         from  point_record_table prt 
         where created_at >current_date - interval '120' day and created_at <current_date - interval '60' day
         group by node_id
         limit 120
         )
         ,curboard as(
         select node_id,count(distinct bot_log_id) total
         from  point_record_table prt 
         where created_at >current_date - interval '60' day 
         group by node_id
         limit 120
         )
         select *
         from lastboard 
         where not exists (select node_id
         from curboard)"""
dat2 = sqlio.read_sql_query(query2, conn)
dat2.to_csv(DEPARTING_NODE_RECORD_CSV, index=False)
logger.info('departing_node_record csv file generated')


message = Mail(from_email=BaseConfig.FROM_EMAIL,
               to_emails=BaseConfig.TO_EMAILS,
               subject=BaseConfig.SUBJECT,
               plain_text_content=BaseConfig.PLAIN_TEXT)

with open(LEADERBOARD_RECORDS_CSV, 'rb') as fd:
    data = fd.read()
    fd.close()
b64data = base64.b64encode(data)
attach_file = Attachment(
    FileContent(str(b64data, 'utf-8')),
    FileName(LEADERBOARD_RECORDS_CSV),
    FileType('application/csv'),
    Disposition('attachment')
)

with open(DEPARTING_NODE_RECORD_CSV, 'rb') as fd:
    data = fd.read()
    fd.close()
b64data = base64.b64encode(data)
departing_node_attach_file = Attachment(
    FileContent(str(b64data, 'utf-8')),
    FileName(DEPARTING_NODE_RECORD_CSV),
    FileType('application/csv'),
    Disposition('attachment')
)
message.attachment = attach_file
message.attachment = departing_node_attach_file


try:
    sg = SendGridAPIClient(api_key=BaseConfig.SENDGRID_API_KEY)
    response = sg.send(message)
    logger.info(response.status_code)
except Exception as e:
    logger.info(e)