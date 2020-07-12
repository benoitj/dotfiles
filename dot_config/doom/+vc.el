;;; ~/src/dotfiles/emacs/.config/doom/+vc.el -*- lexical-binding: t; -*-


(use-package! git-link
  :commands
  (git-link git-link-commit git-link-open-in-browser)
  :init (setq git-link-open-in-browser t))

(after! magit
  :config
 (setq magit-diff-refine-hunk 'all))
