;;; ~/src/dotfiles/emacs/.config/doom/+term.el -*- lexical-binding: t; -*-

(map! :leader (:prefix "o"
                (:prefix ("s" . "shell")
                  :desc "better-rsh" "r" #'better-shell-remote-open
                  :desc "eshell" "e" #'eshell
                  :desc "better-sh" "s" #'better-shell-shell
                  )))

(use-package! keychain-environment
  :config
  (keychain-refresh-environment))
