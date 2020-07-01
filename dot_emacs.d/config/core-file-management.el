;;; -*- lexical-binding: t -*-
;;; DONE


(global-set-key (kbd "<f5>") 'revert-buffer)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package neotree
  :bind
  (([f8] . 'neotree-toggle)))


;; Disk space is cheap. Save lots. (c) Sacha Chua
;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      delete-old-versions -1
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
      undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/")))

;; But don't create stupid lockfiles
(setq create-lockfiles nil)

;; History
(setq savehist-file "~/.emacs.d/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring)
      recentf-max-saved-items 50)

(savehist-mode 1)
(recentf-mode 1)

;; enable open dired for current buffer
(require 'dired-x)
;; allow dired to delete or copy dir
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

(defun my/dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'my/dired-mode-setup)
