FROM php:apache
RUN apt-get update

COPY --from=mlocati/php-extension-installer /usr/bin/install-php-extensions /usr/local/bin/
RUN install-php-extensions pdo_pgsql pgsql
RUN mv $PHP_INI_DIR/php.ini-development $PHP_INI_DIR/php.ini

COPY ./php.ini $PHP_INI_DIR/conf.d/
COPY . /var/www/html/
WORKDIR /var/www/html/

# Use the default production configuration
EXPOSE 80
EXPOSE 443
