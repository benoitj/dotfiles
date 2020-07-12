#!/bin/bash

verify_app() {
  APP=$1

  #echo "app: $APP"  

  if [[ ! -f $(which $APP) ]]; then
    notify-send $(basename $0) "Program $APP is missing"
    false
  fi

}

yes_no() {
  yesno=`echo -e "No\nYes" | $DMENU -p "$1" -i`
  
  if test "$yesno" = "Yes"
  then
    return  1;
  fi
  return  0;
}
