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

#udiskie -a -N &
dwmstatus &

test -p /tmp/dwm.fifo || (umask 077; mkfifo /tmp/dwm.fifo)
exec dbus-launch dwm
