#!/bin/bash

url="$1"   #url
title="$2" # tags
description="$3" #nickname (single word only, no spaces)

echo -e "${description}\t${url}\t;; newsboat ${title}" >> ~/.config/surfraw/bookmarks
