from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
import base64
from payouts_config import BaseConfig
import psycopg2
import pandas as pd
from logger_util import logger


logger.info('mail to foundation account')
connection_leaderboard = psycopg2.connect(
    host=BaseConfig.POSTGRES_LEADERBOARD_HOST,
    port=BaseConfig.POSTGRES_LEADERBOARD_PORT,
    database=BaseConfig.POSTGRES_LEADERBOARD_DB,
    user=BaseConfig.POSTGRES_LEADERBOARD_USER,
    password=BaseConfig.POSTGRES_LEADERBOARD_PASSWORD
)

BLOCKS_CSV = 'blocks_won.csv'
def postgresql_to_dataframe(conn):
    # get records where blocks_won is 0
    select_query = """select provider_pub_key,winner_pub_key,blocks  from payout_summary where blocks=0;"""
    cursor = conn.cursor()
    try:
        cursor.execute(select_query)
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0} ", format(error))
        cursor.close()
        return 1

    tuples = cursor.fetchall()
    cursor.close()
    column_names = ['provider_pub_key', 'winner_pub_key', 'blocks']
    # We just need to turn it into a pandas dataframe
    df = pd.DataFrame(tuples, columns=column_names)
    return df


def mail_to_foundation_accounts(zero_block_producers, epoch_no):
    blocks_df = zero_block_producers
    blocks_df.to_csv(BLOCKS_CSV)

    message = Mail(from_email=BaseConfig.FROM_EMAIL,
                   to_emails=BaseConfig.PROVIDER_EMAIL,
                   subject='Zero block producers for epoch '+str(epoch_no),
                   plain_text_content='Please see the attached list of zero block producers',
                   html_content='<p> Hi, please find the attachment Below </p>')

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
