;;; -*- lexical-binding: t -*-

(require 'cc-mode)

(use-package yasnippet)
(use-package lsp-mode)
(use-package hydra)
(use-package company-lsp)
(use-package lsp-ui)
(use-package lsp-java :after lsp
  :config
  (require 'lsp-java-boot)
  (add-hook 'java-mode-hook 'lsp)  
  (add-hook 'java-mode-hook 'lsp-lens-mode)  
  (add-hook 'java-mode-hook 'lsp-java-boot-lens-mode)  
  )

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (require 'dap-java))

;;(use-package dap-java :after (lsp-java))
