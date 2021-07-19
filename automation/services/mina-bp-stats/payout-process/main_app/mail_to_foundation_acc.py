from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
import base64
from payouts_config import BaseConfig
import psycopg2
import pandas as pd
from logger_util import logger


BLOCKS_CSV = 'blocks_won.csv'

def mail_to_foundation_accounts(zero_block_producers, epoch_no):
    blocks_df = zero_block_producers
    blocks_df.to_csv(BLOCKS_CSV)

    message = Mail(from_email=BaseConfig.FROM_EMAIL,
                   to_emails=BaseConfig.PROVIDER_EMAIL,
                   subject='Zero block producers for epoch '+str(epoch_no),
                   plain_text_content='Please find the attached list of zero block producers',
                   html_content='<p> Please find the attached list of zero block producers </p>')

    with open(BLOCKS_CSV, 'rb') as fd:
        data = fd.read()
        fd.close()
    b64data = base64.b64encode(data)
    attch_file = Attachment(
        FileContent(str(b64data, 'utf-8')),
        FileName(BLOCKS_CSV),
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
        logger.info(e)
