#!/bin/bash 

ALSPACDIR=~/alspac/data

## build the container in the current directory:
docker build --build-arg WHEN=2020-06-22 -t alspac-r-test .

## run the tests
CURRENTDIR=`pwd`
docker run -v $ALSPACDIR:/home/alspac -v $CURRENTDIR:/home/example alspac-r-test /bin/bash -c "cd /home/example; make all"
