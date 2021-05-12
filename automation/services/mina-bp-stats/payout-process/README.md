## Payout process  
Initial config for Payout process
  
  
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
	Copy the GCS Credentials JSON file to local folder as payouts_calculate.py script, 
	and update the file name in config.py "CREDENTIAL_PATH"  
	
	`CREDENTIAL_PATH`		**Required** JSON file generated for GCS credentials.  
    `GCS_BUCKET_NAME`		**Required** GCS Bucket name  
	  
## Email Credentials Config:	  
	Update below in config.py
	`SENDGRID_API_KEY`	**Required** Sendgrid API secret key.  
    `FROM_EMAIL`		**Required** From email to be used.
	`TO_EMAILS`			**Required** list of comma separeted email id's to send email to.
	

***
### Installing Docker file
1. Go to the terminal.
2. Type below Commands.
3. * >cd payout-process
   * >docker build -t payout-process .
   * >docker run -i -t payout-process:latest
    
For docker, please update all above properties also copy the credentials file in same folder as 	  
	  
	