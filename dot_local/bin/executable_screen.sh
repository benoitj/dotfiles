#!/bin/bash

mode="--right-of"

if test "$1" == "-m"
then
	mode="--same-as"
fi

primary=`xrandr -q | grep " connected" | grep primary | awk -e '{print $1}'`
additional=`xrandr -q | grep " connected" | grep -v primary | awk -e '{print $1}'`

if test "$additional" == ""
then
	exit
fi

#xrandr --listactivemonitors | grep -q "$additional"
#
#if test $? -eq 0
#then
#	exit
#fi

echo "additional: $additional"

xrandr --output $additional --auto $mode $primary
	
