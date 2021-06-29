#!/bin/bash

kill -s SIGUSR2 $(/find_puppeteer.sh)
while [ ! -f daemon-active ]; do sleep 1; done