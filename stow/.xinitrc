#!/run/current-system/profile/bin/bash

. /home/benoit/.guix-extra-profiles/desktop/desktop/etc/profile

/home/benoit/.guix-extra-profiles/desktop/desktop/etc/profile/bin/sxhkd -m -1 &

test -p /tmp/dwm.fifo || (umask 077; mkfifo /tmp/dwm.fifo)

sleep 10

exec /home/benoit/.guix-extra-profiles/desktop/desktop/etc/profile/bin/dwm
