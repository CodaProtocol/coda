import psycopg2
from datetime import timezone
import datetime
from payouts_config import BaseConfig
import pandas as pd
from sendgrid import SendGridAPIClient
from sendgrid.helpers.mail import Mail, Attachment, FileContent, FileName, FileType, Disposition
from logger_util import logger

connection_leaderboard = psycopg2.connect(
    host=BaseConfig.POSTGRES_LEADERBOARD_HOST,
    port=BaseConfig.POSTGRES_LEADERBOARD_PORT,
    database=BaseConfig.POSTGRES_LEADERBOARD_DB,
    user=BaseConfig.POSTGRES_LEADERBOARD_USER,
    password=BaseConfig.POSTGRES_LEADERBOARD_PASSWORD
)

logger.info('validate email send to accounts ')


def get_block_producer_mail(winner_bpk, conn=connection_leaderboard):
    mail_id_sql = """select block_producer_email from node_record_table where block_producer_key = %s"""
    cursor = conn.cursor()
    try:
        cursor.execute(mail_id_sql, (winner_bpk,))
    except (Exception, psycopg2.DatabaseError) as error:
        logger.info("Error: {0} ", format(error))
        cursor.close()
        return 1
    data = cursor.fetchall()
    #email = data[-1][-1]
    email = "umesh.bihani@bnt-soft.com"
    return email


def second_mail(email_df, epoch_id):
    payouts_df = email_df
    total_minutes = (epoch_id * 7140 * 3) + (BaseConfig.SLOT_WINDOW_VALUE * 3)
    deadline_date = BaseConfig.GENESIS_DATE + datetime.timedelta(minutes=total_minutes)
    deadline_date = deadline_date.strftime("%d-%m-%Y %H:%M:%S")

    # reading email template
    f = open("validate_email_template.txt", "r")
    html_text = f.read()

    count = 1

    for i in range(payouts_df.shape[0]):
        count = count + 1
        # 0- provider_pub_key, 1- winner_pub_key, 2- payout_amount
        html_content2 = html_text
        # Adding dynamic values into the template
        html_content2 = html_content2.replace("#FOUNDATION_ADDRESS", str(payouts_df.iloc[i, 0]))
        html_content2 = html_content2.replace("#PAYOUT_AMOUNT", str(payouts_df.iloc[i, 2]))
        html_content2 = html_content2.replace("#BLOCK_PRODUCER_ADDRESS", str(payouts_df.iloc[i, 1]))
        html_content2 = html_content2.replace("#DEADLINE_DATE", str(deadline_date))

        subject = "🚨[ACTION REQUIRED ] 24 HOURS LEFT to Send Block Rewards in MINAS for the Delegation"
        block_producer_email = get_block_producer_mail(payouts_df.iloc[i, 1])

        message = Mail(from_email=BaseConfig.FROM_EMAIL,
                       to_emails=block_producer_email,
                       subject=subject,
                       plain_text_content='text',
                       html_content=html_content2)

        try:
            sg = SendGridAPIClient(api_key=BaseConfig.SENDGRID_API_KEY)
            response = sg.send(message)
            logger.info(response.status_code)
            logger.info(response.body)
            logger.info(response.headers)
        except Exception as e:
            logger.error(e)
    logger.info("Calculation: epoch number: {0}, emails sent: {1}".format(epoch_id,count))