import datetime
import logging


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.DEBUG
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_ARCHIVE_HOST = 'minanetdb.ctldjwfweirp.us-east-1.rds.amazonaws.com'
    POSTGRES_ARCHIVE_PORT = 5432
    POSTGRES_ARCHIVE_USER = 'mina_admin'
    POSTGRES_ARCHIVE_PASSWORD = 'mina2021!'
    POSTGRES_ARCHIVE_DB = 'archive'

    POSTGRES_PAYOUT_HOST = 'localhost'
    POSTGRES_PAYOUT_PORT = 5432
    POSTGRES_PAYOUT_USER = 'postgres'
    POSTGRES_PAYOUT_PASSWORD = 'postgres'
    POSTGRES_PAYOUT_DB = 'payout_o1labs'

    POSTGRES_LEADERBOARD_HOST = 'minanetdb.ctldjwfweirp.us-east-1.rds.amazonaws.com'
    POSTGRES_LEADERBOARD_PORT = 5432
    POSTGRES_LEADERBOARD_USER = 'mina_admin'
    POSTGRES_LEADERBOARD_PASSWORD = 'mina2021!'
    POSTGRES_LEADERBOARD_DB = 'leaderboard'
    COINBASE = 720
    SLOT_WINDOW_VALUE = 3500
    CREDENTIAL_PATH = 'mina-mainnet-303900-45050a0ba37b.json'
    API_KEY = ''
    GCS_BUCKET_NAME = 'mina-staking-ledgers'
    FROM_EMAIL = 'mehul.wankhede@bnt-soft.com'
    OVERRIDE_EMAIL='umesh@ontab.com'
    PROVIDER_EMAIL = ['umesh@ontab.com']
    TO_EMAILS = ['umesh@ontab.com']
    SUBJECT = 'LeaderBoard Stats As of{0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = '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'
    SPREADSHEET_SCOPE = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
    SPREADSHEET_NAME = 'Mina Foundation Delegation Application (Responses)'
    DELEGATION_ADDRESSS_CSV='O1_Labs_addresses_1.csv'
    GENESIS_DATE = datetime.datetime(2021, 3, 17)
