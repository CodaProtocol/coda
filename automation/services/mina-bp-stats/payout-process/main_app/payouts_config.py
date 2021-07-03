import datetime
import logging


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.WARN
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_ARCHIVE_HOST = '172.31.21.48'
    POSTGRES_ARCHIVE_PORT = 5432
    POSTGRES_ARCHIVE_USER = 'minanetuser'
    POSTGRES_ARCHIVE_PASSWORD = 'minanetuser'
    POSTGRES_ARCHIVE_DB = 'archive'

    POSTGRES_PAYOUT_HOST = '172.31.21.48'
    POSTGRES_PAYOUT_PORT = 5432
    POSTGRES_PAYOUT_USER = 'minanetuser'
    POSTGRES_PAYOUT_PASSWORD = 'minanetuser'
    POSTGRES_PAYOUT_DB = 'minanet_payout'

    POSTGRES_LEADERBOARD_HOST = '172.31.21.48'
    POSTGRES_LEADERBOARD_PORT = 5432
    POSTGRES_LEADERBOARD_USER = 'minanetuser'
    POSTGRES_LEADERBOARD_PASSWORD = 'minanetuser'
    POSTGRES_LEADERBOARD_DB = 'minanetdb'
    COINBASE = 720
    SLOT_WINDOW_VALUE = 3500
    CREDENTIAL_PATH = 'mina-mainnet-303900-45050a0ba37b.json'
    API_KEY = ''
    GCS_BUCKET_NAME = 'mina-staking-ledgers'
    FROM_EMAIL = 'umesh@ontab.com'
    OVERRIDE_EMAIL='umesh@ontab.com'
    PROVIDER_EMAIL = ['umesh@ontab.com']
    TO_EMAILS = ['umesh@ontab.com']
    SUBJECT = 'LeaderBoard Stats As of{0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = ''
    SPREADSHEET_SCOPE = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
    SPREADSHEET_NAME = 'Mina Foundation Delegation Application (Responses)'
    DELEGATION_ADDRESSS_CSV='O1_Labs_addresses_1.csv'
    GENESIS_DATE = datetime.datetime(2021, 3, 17)
