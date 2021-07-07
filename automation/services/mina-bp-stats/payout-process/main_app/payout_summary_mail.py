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

PAYOUT_SUMMARY_INFO = 'payout_summary_info_'
ERROR = 'Error: {0}'



def payout_summary_mail(payout_df, epoch_no):
    logger.info('sending payout summary mail to foundation account')
    csv_name=PAYOUT_SUMMARY_INFO+str(epoch_no)+'.csv'
    payout_df.to_csv(csv_name)

    message = Mail(from_email=BaseConfig.FROM_EMAIL,
                   to_emails=BaseConfig.PROVIDER_EMAIL,
                   subject='Payout Summary Details for epoch ' + str(epoch_no),
                   plain_text_content='Please find the attached list of payout summary details',
                   html_content='<p> Please find the attached list of payout summary details </p>')

    with open(csv_name, 'rb') as fd:
        data = fd.read()
        fd.close()
    b64data = base64.b64encode(data)
    attch_file = Attachment(
        FileContent(str(b64data, 'utf-8')),
        FileName(csv_name),
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
