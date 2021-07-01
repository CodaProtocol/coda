from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
import base64
from payouts_config import BaseConfig
import psycopg2
import pandas as pd
from logger_util import logger


connection_payout = psycopg2.connect(
    host=BaseConfig.POSTGRES_PAYOUT_HOST,
    port=BaseConfig.POSTGRES_PAYOUT_PORT,
    database=BaseConfig.POSTGRES_PAYOUT_DB,
    user=BaseConfig.POSTGRES_PAYOUT_USER,
    password=BaseConfig.POSTGRES_PAYOUT_PASSWORD
)

PAYOUT_SUMMARY_INFO = 'payout_summary_info.csv'
ERROR = 'Error: {0}'

def get_payout_data(conn):
    select_query = """select  provider_pub_key, winner_pub_key, blocks, payout_amount, payout_balance, 
    last_delegation_epoch from payout_summary"""
    cursor = conn.cursor()
    try:
        cursor.execute(select_query)
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0} ", format(error))
        cursor.close()
        return 1

    tuples = cursor.fetchall()
    cursor.close()
    column_names = ['provider_pub_key', 'winner_pub_key', 'blocks_produced', 'payout_amount', 'payout_balance',
                    'last_delegation_epoch']
    df = pd.DataFrame(tuples, columns=column_names)
    return df


def payout_summary_mail(epoch_no):
    logger.info('sending payout summary mail to foundation account')
    payout_summary_df = get_payout_data(connection_payout)
    payout_summary_df.to_csv(PAYOUT_SUMMARY_INFO)

    message = Mail(from_email=BaseConfig.FROM_EMAIL,
                   to_emails=BaseConfig.PROVIDER_EMAIL,
                   subject='Payout Summary Details for epoch ' + str(epoch_no),
                   plain_text_content='Please find the attached list of payout summary details',
                   html_content='<p> Please find the attached list of payout summary details </p>')

    with open(PAYOUT_SUMMARY_INFO, 'rb') as fd:
        data = fd.read()
        fd.close()
    b64data = base64.b64encode(data)
    attch_file = Attachment(
        FileContent(str(b64data, 'utf-8')),
        FileName(PAYOUT_SUMMARY_INFO),
        FileType('application/csv'),
        Disposition('attachment')
    )

    message.attachment = attch_file

    try:
        sg = SendGridAPIClient(api_key=BaseConfig.SENDGRID_API_KEY)
        response = sg.send(message)
        logger.info(response.status_code)
        logger.info(response.body)
        logger.info(response.headers)
    except Exception as e:
        logger.error(ERROR.format(e))
