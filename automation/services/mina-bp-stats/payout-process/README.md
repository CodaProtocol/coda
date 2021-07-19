## Payout process  
Initial config for Payout process
The application relies on three databases:
     - Mainnet Archive : Uses this to get number of blocks produced and payout transaction details
     - Leaderboard     : Uses this to get Validator/Block producers email addresses
     - Payout          : Uses this to keep track of Payout processing
Use payout_schema.sql to create Payout database

## DB Config:   
	Install postgres   
	Execute SQL statement from payout_schema.sql. This will create tables and initial config data.  

	In payouts_config.py update below properties (All properties are required):  
    Mainnet Archive DB configurations:
	`POSTGRES_ARCHIVE_HOST`			The postgres hostname  
    `POSTGRES_ARCHIVE_PORT`			The postgres port  
    `POSTGRES_ARCHIVE_USER`			The postgres username  
    `POSTGRES_ARCHIVE_PASSWORD`		The postgres password  
    `POSTGRES_ARCHIVE_DB`			The postgres  database name  
	
    Similarly, for Payout DB configurations:
    `POSTGRES_PAYOUT_HOST` 
    `POSTGRES_PAYOUT_PORT` 
    `POSTGRES_PAYOUT_USER` 
    `POSTGRES_PAYOUT_PASSWORD`  
    `POSTGRES_PAYOUT_DB` 

    And lastly, Leaderboard DB configurations:
    `POSTGRES_LEADERBOARD_HOST` 
    `POSTGRES_LEADERBOARD_PORT` 
    `POSTGRES_LEADERBOARD_USER`  
    `POSTGRES_LEADERBOARD_PASSWORD`  
    `POSTGRES_LEADERBOARD_DB` 
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
## Setup the env-file
1.Add all required credential value to file as key-value pair.Review the payout_config_variables.env file. 
2.Provide this .env file while running the docker run command.
### Installing Docker file
1. Go to the terminal.
2. Type below Commands.
3. * >cd payout-process
   * >docker build -t payout-process .
   * >docker run --env-file payout_config_variables.env -i -t payout-process:latest
    
For docker, please update all above properties also copy the credentials file in same folder as 	  
	  
	