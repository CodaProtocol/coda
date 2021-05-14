import datetime
import logging


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.WARN
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_ARCHIVE_HOST = '127.0.0.1'
    POSTGRES_ARCHIVE_PORT = 5432
    POSTGRES_ARCHIVE_USER = 'postgres'
    POSTGRES_ARCHIVE_PASSWORD = 'postgres'
    POSTGRES_ARCHIVE_DB = 'mainnet_archive'

    POSTGRES_PAYOUT_HOST = '127.0.0.1'
    POSTGRES_PAYOUT_PORT = 5432
    POSTGRES_PAYOUT_USER = 'postgres'
    POSTGRES_PAYOUT_PASSWORD = 'postgres'
    POSTGRES_PAYOUT_DB = 'minanet_payout'

    POSTGRES_LEADERBOARD_HOST = '127.0.0.1'
    POSTGRES_LEADERBOARD_PORT = 5432
    POSTGRES_LEADERBOARD_USER = 'postgres'
    POSTGRES_LEADERBOARD_PASSWORD = 'postgres'
    POSTGRES_LEADERBOARD_DB = 'minanetdb'
    COINBASE = 720
    SLOT_WINDOW_VALUE = 3500
    CREDENTIAL_PATH = 'mina-mainnet-303900-45050a0ba37b.json'
    API_KEY = 'AIzaSyA3z01DZpfHFDq5Ln1nnebOeJ2aXElvd1Y'
    GCS_BUCKET_NAME = 'mina-staking-ledgers'
    FROM_EMAIL = 'ruchika.jain@bnt-soft.com'
    PROVIDER_EMAIL = ['umesh.bihani@bnt-soft.com']
    TO_EMAILS = ['umesh.bihani@bnt-soft.com']
    SUBJECT = 'LeaderBoard Stats As of{0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = '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'
    SPREADSHEET_SCOPE = ['https://spreadsheets.google.com/feeds', 'https://www.googleapis.com/auth/drive']
    SPREADSHEET_NAME = 'Mina Foundation Delegation Application (Responses)'
    GENESIS_DATE = datetime.datetime(2021, 3, 17)
