;;; -*- lexical-binding: t -*-
;;; DONE


(use-package elfeed
  :config
  (global-set-key (kbd "C-x w") 'elfeed))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files '("~/Documents/elfeed.org"))
  (elfeed-org))

(use-package elfeed-goodies
  :after elfeed)

(use-package mastodon
  :config
  (setq mastodon-instance-url "https://linuxrocks.online"))

