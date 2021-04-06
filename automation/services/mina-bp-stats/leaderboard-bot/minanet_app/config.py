import logging
import datetime


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.WARN
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_HOST = '127.0.0.1'
    POSTGRES_PORT = 5432
    POSTGRES_USER = 'postgres'
    POSTGRES_PASSWORD = 'postgres'
    POSTGRES_DB = 'minanetdb'
    SQLALCHEMY_DATABASE_URI = f'postgresql://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:{POSTGRES_PORT}/{POSTGRES_DB}'
    CREDENTIAL_PATH = 'gcs-credential.json'
    GCS_BUCKET_NAME = 'block-producer-stats'
    READ_FILE_INTERVAL = 10
    FROM_EMAIL = ''
    TO_EMAILS = ['', '']
    SUBJECT = 'Ontab-key LeaderBoard positions As of {0}'.format(datetime.datetime.utcnow())
    PLAIN_TEXT = 'Report for Leaderboard as of {0}'.format(datetime.datetime.utcnow())
    SENDGRID_API_KEY = ''
