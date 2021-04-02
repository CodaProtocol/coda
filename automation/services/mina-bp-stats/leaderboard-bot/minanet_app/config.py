import logging
from datetime import datetime, timedelta


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
    CREDENTIAL_PATH = 'mina-mainnet-303900-45050a0ba37b.json'
    GCS_BUCKET_NAME = 'block-producer-stats'
    READ_FILE_INTERVAL = 10