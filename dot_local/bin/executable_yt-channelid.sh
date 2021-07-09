#!/bin/bash

# this script give the yt channel id from any url

URL=$1

test -n $URL || exit 1;

youtube-dl -j "$URL" | jq '.uploader_id' | sed -e 's/"//g'
