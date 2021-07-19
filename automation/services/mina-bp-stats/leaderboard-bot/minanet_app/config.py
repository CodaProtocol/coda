import logging
import datetime
import os

class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.WARN
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_HOST = str(os.environ['POSTGRES_HOST']).strip()
    POSTGRES_PORT = int(os.environ['POSTGRES_PORT'])
    POSTGRES_USER = str(os.environ['POSTGRES_USER']).strip()
    POSTGRES_PASSWORD = str(os.environ['POSTGRES_PASSWORD']).strip()
    POSTGRES_DB = os.environ['POSTGRES_DB']
    SQLALCHEMY_DATABASE_URI = f'postgresql://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:{POSTGRES_PORT}/{POSTGRES_DB}'
    CREDENTIAL_PATH = 'leaderboard-bot\minanet_app\mina-mainnet-303900-45050a0ba37b.json'
    GCS_BUCKET_NAME = str(os.environ['GCS_BUCKET_NAME']).strip()
    PROVIDER_ACCOUNT_PUB_KEYS_FILE = 'Mina_Foundation_Addresses.csv'
    SURVEY_INTERVAL_MINUTES = int(os.environ['SURVEY_INTERVAL_MINUTES'])
    UPTIME_DAYS_FOR_SCORE = int(os.environ['UPTIME_DAYS_FOR_SCORE'])
    FROM_EMAIL = str(os.environ['FROM_EMAIL']).strip()
    TO_EMAILS = os.environ['TO_EMAILS'].split(',')
    SUBJECT = 'Ontab-key LeaderBoard positions As of {0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = str(os.environ['SENDGRID_API_KEY']).strip()
    SPREADSHEET_SCOPE = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
    SPREADSHEET_NAME = str(os.environ['SPREADSHEET_NAME']).strip()
    SPREADSHEET_JSON = 'leaderboard-bot\minanet_app\mina-foundation-delegation-app-b889c28d5b9b.json'
    MAX_THREADS_TO_DOWNLOAD_FILES= int(os.environ["MAX_THREADS_TO_DOWNLOAD_FILES"])
