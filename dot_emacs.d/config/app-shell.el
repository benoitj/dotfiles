;;; -*- lexical-binding: t -*-
;;; DONE

(use-package better-shell
  :bind (("C-'" . better-shell-shell)
	 ("C-;" . better-shell-remote-open)))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))
