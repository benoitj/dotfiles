#!/bin/bash

action=`echo "Sleep|Logout|Reboot|Shutdown|Switch User" | rofi -dmenu -sep \| -i -p "Action: "`

if test "$action" == ""
then
  exit 1
fi

if test "$action" == "Sleep"
then
  systemctl suspend
  exit 0
fi

if test "$action" == "Switch User"
then
  dm-tool switch-to-greeter
  exit 0
fi

if test "$action" == "Reboot"
then
  yesno=`echo "No|Yes" | rofi -dmenu -sep \| -p Reboot? -i`
  
  if test "$yesno" == "Yes"
  then
    reboot
  fi
  exit 0
fi

if test "$action" == "Shutdown"
then
  yesno=`echo "No|Yes" | rofi -dmenu -sep \| -p Shutdown? -i`
  
  if test "$yesno" == "Yes"
  then
    shutdown now
  fi
  exit 0
fi

if test "$action" == "Logout"
then
  yesno=`echo "No|Yes" | rofi -dmenu -sep \| -p Logout? -i`
  
  if test "$yesno" == "Yes"
  then
    i3-msg exit
  fi
  exit 0
fi


