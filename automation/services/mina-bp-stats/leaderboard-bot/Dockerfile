FROM python:latest
WORKDIR /project

COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY mina-mainnet-303900-b9056c625e58.json mina-mainnet-303900-b9056c625e58.json
COPY download_batch_files.py download_batch_files.py
COPY config.py config.py
COPY surver_collect.py surver_collect.py
CMD ["python", "surver_collect.py"]