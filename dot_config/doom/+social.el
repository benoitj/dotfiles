;;; ~/src/dotfiles/emacs/.config/doom/+social.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "o"
        :desc "elfeed" "e" #'=rss))

(use-package! elfeed-org
  :after elfeed
  :init
  (setq rmh-elfeed-org-files '("~/src/notebook/elfeed.org")))

(use-package! elfeed-goodies
  :after elfeed)

;;(use-package mastodon
;;  :config
;;  (setq mastodon-instance-url "https://linuxrocks.online"))
