## Leaderboard Bot
Initial config for Leaderboard Bot

##DB Config:
	Install postgres
	Execute SQL statement from database\tables.sql. This will create tables and initial config data needed by bot.
	In config.py update below properties (All properties are required):
	`POSTGRES_HOST`			The postgres hostname
    `POSTGRES_PORT`			The postgres port
    `POSTGRES_USER`			The postgres username
    `POSTGRES_PASSWORD`		The postgres password
    `POSTGRES_DB`			 The postgres  database name
	**Note**  If postgres is hosted on different machine, make sure to update "postgresql.conf" and set  "listen_addresses" to appropriate value.
##GCS Credentials Config:	
	Copy theGCS Credentials JSON file to local folder as survey_collect.py script, and update the file name in config.py "CREDENTIAL_PATH"
	`CREDENTIAL_PATH`		**Required** JSON file generated for GCS credentials.
    `GCS_BUCKET_NAME`		**Required** GCS Bucket name, when 
	
	
	