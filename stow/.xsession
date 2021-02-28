#!/run/current-system/profile/bin/bash

source $HOME/.bash_profile

sxhkd -m -1 &

test -p /tmp/dwm.fifo || (umask 077; mkfifo /tmp/dwm.fifo)

st &
exec /home/benoit/.guix-extra-profiles/desktop/desktop/bin/dwm
