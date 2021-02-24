;;; +notmuch.el -*- lexical-binding: t; -*-

(setq
 user-full-name "Benoit Joly"
 user-mail-address "benoit@benoitj.ca"
 user-mail-addresses '("benoit@benoitj.ca" "benoit.m.joly@gmail.com" "bjoly666@gmail.com")
;; smtpmail-smtp-server "smtp.fastmail.com"
;; smtpmail-smtp-service 587
;; smtpmail-stream-type 'starttls
;; smtpmail-smtp-service 465
;; smtpmail-stream-type 'ssl
 )

(setq message-kill-buffer-on-exit nil)

(map! :leader :desc "notmuch" "o m" #'notmuch)

(after! notmuch
  (map! :map notmuch-show-mode-map :localleader :desc "browse urls" "b" #'notmuch-show-browse-urls))
