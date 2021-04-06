## Leaderboard Bot  
Initial config for Leaderboard Bot  
  
  
## DB Config:   
	Install postgres   
	Execute SQL statement from database\tables.sql. This will create tables and initial config data needed by bot.  
	In config.py update below properties (All properties are required):  
	`POSTGRES_HOST`			The postgres hostname  
    `POSTGRES_PORT`			The postgres port  
    `POSTGRES_USER`			The postgres username  
    `POSTGRES_PASSWORD`		The postgres password  
    `POSTGRES_DB`			 The postgres  database name  
	
	**Note**  If postgres is hosted on different machine, make sure to update "postgresql.conf" 
		and set  "listen_addresses" to appropriate value.  
	  
  
## GCS Credentials Config:	  
	Copy the GCS Credentials JSON file to local folder as survey_collect.py script, 
	and update the file name in config.py "CREDENTIAL_PATH"  
	
	`CREDENTIAL_PATH`		**Required** JSON file generated for GCS credentials.  
    `GCS_BUCKET_NAME`		**Required** GCS Bucket name  
	  
## Email Credentials Config:	  
	Update below in config.py
	`SENDGRID_API_KEY`	**Required** JSON file generated for GCS credentials.  
    `FROM_EMAIL`		**Required** GCS Bucket name 
	`TO_EMAILS`			**Required** GCS Bucket name 
	

***
### Installing Docker file
1. Go to the terminal.
2. Type belowe Commands.
3. * >cd leaderboard-bot
   * >docker build -t leaderborad-bot .
   * >docker run -i -t leaderborad-bot:latest
    
For docker, please update all above propeties also copy the credetials file in same folder as 	  
	  
	