;;; tools/git-extras/config.el -*- lexical-binding: t; -*-

(use-package! git-link
  :commands
  (git-link git-link-commit git-link-open-in-browser)
  :init (setq git-link-open-in-browser t))

(map! :leader
      (:prefix "g"
      :desc "git-link" "x" #'git-link))
