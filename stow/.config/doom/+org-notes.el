;;; ~/src/dotfiles/emacs/.config/doom/+org-notes.el -*- lexical-binding: t; -*-
;;;

;; TODO review if I want to keep it
(setq org-roam-directory "~/src/notebook/notes")
(setq org-roam-link-title-format "R:%s")

;; (map! :leader
;;       "n r r" #'org-roam
;;       "n r f" #'org-roam-find-file
;;       "n r g" #'org-roam-show-graph
;;       )
;; (map! :map org-mode-map
;;       :localleader
;;       "l r" #'org-roam-insert)

;;(use-package! deft
;; :after org
;;  :custom
;;  (deft-recursive t)
;;  (deft-use-filter-string-for-filename t)
;;  (deft-default-extension "org")
;;  (deft-directory "~/src/notebook"))
