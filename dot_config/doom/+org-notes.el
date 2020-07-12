;;; ~/src/dotfiles/emacs/.config/doom/+org-notes.el -*- lexical-binding: t; -*-
;;;

(use-package! org-roam
  :after org
  :init
  (setq org-roam-directory "~/src/notebook")
  (setq org-roam-link-title-format "R:%s")
;;  :config
;;  (map! :map org-roam-mode-map
;;        :localleader
 ;;       "l" #'org-roam
  ;;      "f" #'org-roam-find-file
  ;;      "g" #'org-roam-show-graph
 ;;       )
;;  :map org-mode-map
;;  :localleader
;;  "i"
;;  (("C-c n i" . org-roam-insert)))
  )

(after! org
  (org-roam-mode))

(map! :leader
      "n r r" #'org-roam
      "n r f" #'org-roam-find-file
      "n r g" #'org-roam-show-graph
      )
(map! :map org-mode-map
      :localleader
      "l r" #'org-roam-insert)

(use-package! deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/src/notebook"))
