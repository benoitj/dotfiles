;;; -*- lexical-binding: t -*-
;;; DONE

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status))
  :config
  (ryo-modal-keys
     ("SPC g" (
   	     ("s" magit-status)
   	     ))

     ))

(use-package diff-hl
  :after magit;
  :hook ((dired-mode . diff-hl-dired-mode)
        (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode t))

(use-package git-link
  :commands (git-link git-link-commit git-link-open-in-browser)
  :custom (git-link-open-in-browser t))

