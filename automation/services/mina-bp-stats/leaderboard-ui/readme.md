# Mina Leaderboard 
### Technologies Used 
***
> Postgresql,
> Html 5,
> Bootstrap 4, 
> Php 8.0,
> Docker

***
### Step to configure postgress Database
>Open File connection.php located in 'web-dev/php/connection.php'. 
##### $username = "your database username";
##### $password = "your database username";
##### $database_name = "your database name";
##### $port = "your database port";
##### $host = "your database Host Ip address";
>configure this variables with your credentials and save the file.
***

***
### Installing Docker file
1. Download / Move WEB-DEV folder to home directory in ubuntu.
2. Go to the terminal.
3. Type belowe Commands.
4. * >cd web-dev/php
   * >docker build -t mina-web .
   * >docker run --rm -p 8080:80 -i -t -d mina-web
  

### Note
After Any changes in project you have rebuild the docker file by using 
`docker build -t mina-web .`
this command and again run the container .
