#!/bin/bash

export PATH=$PATH:~/.local/bin

export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1

# setup usb disk daemon
#udiskie -a -N &

# keyboard config
#setxkbmap dvorak -option ctrl:nocaps  

sxhkd -m -1 &

# authentication agent
$GUIX_EXTRA_PROFILES/desktop/desktop/libexec/polkit-mate-authentication-agent-1 &

# notifications
dunst &

#compositor
compton &

xrdb ~/.Xresources

# hide mous after x time
unclutter &

autorandr -c

# status bar
/usr/local/bin/dwmstatus &

#systemctl --user is-enabled --quiet dbus-broker || systemctl --user enable --quiet dbus-broker
#systemctl --user is-active --quiet dbus-broker || systemctl --user start --quiet dbus-broker

# configure fifo
test -p /tmp/dwm.fifo || (umask 077; mkfifo /tmp/dwm.fifo)

exec dbus-launch /usr/local/bin/dwm
