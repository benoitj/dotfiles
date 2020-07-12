#!/usr/bin/env bash

ENV_FILE=$1

ENV_KEYS=`grep export $ENV_FILE | sed -e 's/export \([^= ]*\)=*.*/$\1/'`

envsubst "$ENV_KEYS" < $2
