FROM ubuntu:18.04

# Run with `docker build --build-arg coda_deb_version=<version>`
ARG coda_deb_version
ARG deb_repo=stable

ENV DEBIAN_FRONTEND noninteractive
RUN echo "Building image with version $coda_deb_version"

# Dependencies
RUN apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y upgrade && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    curl \
    strace \
    dumb-init \
    libssl1.1 \
    libprocps6 \
    libgmp10 \
    libgomp1 \
    libffi6 \
    apt-transport-https \
    ca-certificates \
    dnsutils \
    tzdata && \
  rm -rf /var/lib/apt/lists/*

# archive-node package
RUN echo "deb [trusted=yes] http://packages.o1test.net $deb_repo main" > /etc/apt/sources.list.d/coda.list \
  && apt-get -y update \
  && apt-get install -y "coda-archive=$coda_deb_version"
