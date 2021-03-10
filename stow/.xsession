#!/run/current-system/profile/bin/bash

source $HOME/.bash_profile
$GUIX_EXTRA_PROFILES/desktop/desktop/libexec/polkit-mate-authentication-agent-1 &

xrdb ~/.Xresources
setxkbmap dvorak -option ctrl:nocaps  

sxhkd -m -1 &

compton &
autorandr -c

dunst &
unclutter &

feh --bg-scale Downloads/pexels-philippe-donn-1114690.jpg

#udiskie -a -N &
dwmstatus &

test -x $(which syncthing) && syncthing -logfile=$HOME/.cache/syncthing.log &

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u benoit shepherd)" ]; then
  touch ~/started.pid
  shepherd
fi

test -p /tmp/dwm.fifo || (umask 077; mkfifo /tmp/dwm.fifo)
exec dbus-launch dwm
