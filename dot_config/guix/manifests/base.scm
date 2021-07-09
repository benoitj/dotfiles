;; -*- mode: guix-scheme-*-

(specifications->manifest
 '(
   "coreutils"
   
   ;; network tools
   "borg"
   "curl"
   "rsync"
   "git"
   "bind:utils" ;; nslookup and dig
   "le-certs"   ;; lets encrypt certs
   "nss-certs"  ;; lets encrypt certs

   ;; file mgmt
   "unzip"
   "zip"
   "vim"
   "ranger"
   "bubblewrap"
   "bash-completion"
   ;; firejail

   ;; system monitoring
   "htop"
   "strace"
   "lsof"
   "sysstat"
   ))
