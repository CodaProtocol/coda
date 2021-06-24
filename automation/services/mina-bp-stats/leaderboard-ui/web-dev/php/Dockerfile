FROM php:apache
RUN apt-get update
RUN apt-get install nano
RUN a2enmod ssl && a2enmod rewrite
RUN mkdir -p /etc/apache2/ssl 
COPY ./SSL/*.pem /etc/apache2/ssl/
COPY ./000-default.conf /etc/apache2/sites-available/000-default.conf

# RUN apt-get install -y
COPY --from=mlocati/php-extension-installer /usr/bin/install-php-extensions /usr/local/bin/
RUN install-php-extensions pdo_pgsql pgsql

RUN mv $PHP_INI_DIR/php.ini-development $PHP_INI_DIR/php.ini
# COPY ./php.ini $PHP_INI_DIR/php.ini
COPY ./php.ini $PHP_INI_DIR/conf.d/
COPY . /var/www/html/
WORKDIR /var/www/html/
RUN service apache2 restart
# Use the default production configuration
EXPOSE 80
EXPOSE 443
