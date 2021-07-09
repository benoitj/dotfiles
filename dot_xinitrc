#!/bin/bash

export PATH=$PATH:~/.local/bin

SSH_ENV="$HOME/.ssh/env"

echo "Initialising new SSH agent..."
/usr/bin/ssh-agent | sed 's/^echo/#echo/' >"${SSH_ENV}"
echo succeeded
chmod 600 "${SSH_ENV}"
source "${SSH_ENV}" >/dev/null
/usr/bin/ssh-add -t 216000

export KEYID=0x5425347E4304939A
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# setup usb disk daemon
#udiskie -a -N &

# keyboard config
#setxkbmap dvorak -option ctrl:nocaps

#sxhkd -m -1 &

# authentication agent
/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

# notifications
dunst &

#compositor
compton &

xrdb ~/.Xresources

# hide mous after x time
unclutter &

autorandr -c

# status bar
/usr/bin/dwmstatus &

syncthing --no-browser &

#systemctl --user is-enabled --quiet dbus-broker || systemctl --user enable --quiet dbus-broker
#systemctl --user is-active --quiet dbus-broker || systemctl --user start --quiet dbus-broker

# configure fifo
test -p /tmp/dwm.fifo || (
    umask 077
    mkfifo /tmp/dwm.fifo
)

test -x /usr/bin/dwm && exec dbus-launch /usr/bin/dwm
test -x /usr/local/bin/dwm && exec dbus-launch /usr/local/bin/dwm
