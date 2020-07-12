;;; ~/src/dotfiles/emacs/.config/doom/+lang-adoc.el -*- lexical-binding: t; -*-

(use-package! adoc-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))
