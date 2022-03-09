;;; lang/org-extras/config.el -*- lexical-binding: t; -*-

(use-package! org-appear
  :after org
  :custom
  (org-hide-emphasis-markers t)
  (org-appear-autolinks t)
;;  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-delay 1)
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))
