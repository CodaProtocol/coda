import logging
from logging.handlers import RotatingFileHandler
logging.basicConfig(
        handlers=[RotatingFileHandler('./payout_process.log', maxBytes=52428800, backupCount=10)],
        format="[%(asctime)s] %(levelname)s [%(name)s.%(funcName)s:%(lineno)d] %(message)s",
        datefmt='%Y-%m-%dT%H:%M:%S')

# Creating an object
logger = logging.getLogger()

# Setting the threshold of logger to DEBUG
logger.setLevel(logging.DEBUG)
