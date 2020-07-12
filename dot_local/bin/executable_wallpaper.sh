#!/bin/bash

WALLPAPER=~/.cache/wall.png
WALLPAPERS=~/Nextcloud/wallpapers
PID_FILE=~/.cache/wallpaper.pid

if test "$WALLPAPERS" == ""
then
  echo "Error: missing env variable WALLPAPERS."
  exit 1
fi

if test ! -d "$WALLPAPERS"
then
  echo "Error: path $WALLPAPERS not valid"
  exit 1
fi


if test -f $PID_FILE
then
  ps -p `cat $PID_FILE` > /dev/null
  if test $? == 0
  then
    kill -TERM -`cat $PID_FILE`
  fi
fi

echo $$ > $PID_FILE 

while test 1 == 1
do

#wal -a "90" -c -n -i $WALLPAPER
feh --bg-scale $WALLPAPER 2>/dev/null
randWallpaper=`find "$WALLPAPERS" -type f -name '*.png' -print | sort --random-sort  | head -1`
cp "$randWallpaper" $WALLPAPER
sleep 120 
done

