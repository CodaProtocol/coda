# Mina Leaderboard 
### Technologies Used 
***
> Postgresql,
> Html 5,
> Bootstrap 4, 
> Php 8.0,
> Docker
***

## How to change IP address for Leader board UI
1. >Open File connection.php located in 'web-dev/docker-compose.yml'.

2. > Find Below code and change the subnet Ip with your Config Ip.
```Javascript
networks:
  mina-network:
    ipam:
     driver: default
     config:
      - subnet: "172.16.238.0/24"
      - subnet: "2001:3984:3989::/64"
```
3. >In `ipv4_address` You have to chnage Ip with your Ip.
```Javascript
php:
      ...
      networks:
        mina-network:
          ipv4_address: 172.16.238.10
          ipv6_address: 2001:3984:3989::10
   ```

***
## How to configure postgress Database
>Open File connection.php located in 'web-dev/php/connection.php'. 
##### $username = "your database username";
##### $password = "your database username";
##### $database_name = "your database name";
##### $port = "your database port";
##### $host = "your database Host Ip address";
>configure this variables with your credentials and save the file.
***

### Create SSL certificate 
1. Copy `mkcert-v1.4.3-linux-amd64` file from web-dev folder to your home directory.
2. Type belowe Commands.
   * >chmod +x mkcert-v1.4.3-linux-amd64
   * >./mkcert-v1.4.3-linux-amd64 -install
   * >./mkcert-v1.4.3-linux-amd64 < Host Address name > < your Host Mina Network IP Address > ::1
   * > e.g. `./mkcert-v1.4.3-linux-amd64 mina-project.info 172.16.238.10 ::1`

3. In home directory two files genrated with names `mina-project.info+2.pem` and `mina-project.info+2-key.pem`.
4. Copy this both files in `web-dev/php/SSL`.
5. Rename `mina-project.info+2.pem` with `cert.pem`.
6. Rename `mina-project.info+2-key.pem` with `cert-key.pem`.
***
## Installing Docker file
1. Download / Move WEB-DEV folder, to home directory in ubuntu.
2. Go to the terminal.
3. Type belowe Commands.
4. * >cd web-dev/
   * >docker-compose up -d
### It will install all dependancies and start the container .After finishing the process we will opening the browser with `172.16.238.10`
***

### Note
After Any changes in project you have rebuild the docker file by using 
`docker build -t mina-web .`
this command and again run the container .
