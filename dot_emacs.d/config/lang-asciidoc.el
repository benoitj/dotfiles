;;; -*- lexical-binding: t -*-

(use-package adoc-mode
  :config
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))
