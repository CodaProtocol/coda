import logging
from datetime import datetime, timedelta


class BaseConfig(object):
    DEBUG = False
    TESTING = False
    LOGGING_FORMAT = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    LOGGING_LEVEL = logging.INFO
    LOGGING_LOCATION = 'minanet.log'
    POSTGRES_HOST = '127.0.0.1'
    POSTGRES_PORT = 5432
    POSTGRES_USER = 'postgres'
    POSTGRES_PASSWORD = 'postgres'
    POSTGRES_DB = 'minanetdb'
    SQLALCHEMY_DATABASE_URI = f'postgresql://{POSTGRES_USER}:{POSTGRES_PASSWORD}@{POSTGRES_HOST}:{POSTGRES_PORT}/{POSTGRES_DB}'
    API_KEY = 'AIzaSyA3z01DZpfHFDq5Ln1nnebOeJ2aXElvd1Y'
    script_start_time = datetime.now()
    script_end_time = datetime.now() - timedelta(hours=2)
    read_file_interval = 10
