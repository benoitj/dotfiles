;;; -*- lexical-binding: t -*-

(use-package haskell-mode
  :defer t)

(use-package intero
  :after haskell-mode
  :config
  (intero-global-mode 1))
  
