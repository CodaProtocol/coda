import datetime
import logging
import os


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.WARN
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_ARCHIVE_HOST = str(os.environ['POSTGRES_ARCHIVE_HOST']).strip()
    POSTGRES_ARCHIVE_PORT = int(os.environ['POSTGRES_ARCHIVE_PORT'])
    POSTGRES_ARCHIVE_USER = str(os.environ['POSTGRES_ARCHIVE_USER']).strip()
    POSTGRES_ARCHIVE_PASSWORD = str(os.environ['POSTGRES_ARCHIVE_PASSWORD']).strip()
    POSTGRES_ARCHIVE_DB = str(os.environ['POSTGRES_ARCHIVE_DB']).strip()

    POSTGRES_PAYOUT_HOST = str(os.environ['POSTGRES_PAYOUT_HOST']).strip()
    POSTGRES_PAYOUT_PORT = int(os.environ['POSTGRES_PAYOUT_PORT'])
    POSTGRES_PAYOUT_USER = str(os.environ['POSTGRES_PAYOUT_USER']).strip()
    POSTGRES_PAYOUT_PASSWORD = str(os.environ['POSTGRES_PAYOUT_PASSWORD']).strip()
    POSTGRES_PAYOUT_DB = str(os.environ['POSTGRES_PAYOUT_DB']).strip()

    POSTGRES_LEADERBOARD_HOST = str(os.environ['POSTGRES_LEADERBOARD_HOST']).strip()
    POSTGRES_LEADERBOARD_PORT = int(os.environ['POSTGRES_LEADERBOARD_PORT'])
    POSTGRES_LEADERBOARD_USER = str(os.environ['POSTGRES_LEADERBOARD_USER']).strip()
    POSTGRES_LEADERBOARD_PASSWORD = str(os.environ['POSTGRES_LEADERBOARD_PASSWORD']).strip()
    POSTGRES_LEADERBOARD_DB = str(os.environ['POSTGRES_LEADERBOARD_DB']).strip()

    COINBASE = int(os.environ['COINBASE'])
    SLOT_WINDOW_VALUE = int(os.environ['SLOT_WINDOW_VALUE'])
    CREDENTIAL_PATH = str(os.environ['CREDENTIAL_PATH']).strip()
    API_KEY = str(os.environ['API_KEY']).strip()
    OVERRIDE_EMAIL = os.environ['OVERRIDE_EMAIL']
    GCS_BUCKET_NAME = str(os.environ['GCS_BUCKET_NAME']).strip()
    FROM_EMAIL = str(os.environ['FROM_EMAIL'])
    PROVIDER_EMAIL = os.environ['PROVIDER_EMAIL'].split(',')
    TO_EMAILS = os.environ['TO_EMAILS'].split(',')
    SUBJECT = 'LeaderBoard Stats As of{0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = str(os.environ['SENDGRID_API_KEY']).strip()
    DELEGATION_ADDRESSS_CSV = str(os.environ['DELEGATION_ADDRESSS_CSV']).strip()
    SPREADSHEET_SCOPE = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
    SPREADSHEET_NAME = str(os.environ['SPREADSHEET_NAME']).strip()
    GENESIS_DATE = datetime.datetime(2021, 3, 17)