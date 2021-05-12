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


def mail_to_foundation_accounts():
    blocks_df = postgresql_to_dataframe(connection_leaderboard)
    blocks_df.to_csv('blocks_won.csv')

    message = Mail(from_email=BaseConfig.FROM_EMAIL,
                   to_emails=BaseConfig.PROVIDER_EMAIL,
                   subject='Winner account with 0 blocks Won ',
                   plain_text_content='Hi, please find the attachment Below',
                   html_content='<p> Hi, please find the attachment Below </p>')

    with open('blocks_won.csv', 'rb') as fd:
        data = fd.read()
        fd.close()
    b64data = base64.b64encode(data)
    attch_file = Attachment(
        FileContent(str(b64data, 'utf-8')),
        FileName('blocks_won.csv'),
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
