;;; ~/src/dotfiles/emacs/.config/doom/+log.el -*- lexical-binding: t; -*-

(use-package! command-log-mode
  :defer t
  :commands (command-log-mode global-command-log-mode)
  )

(map! :leader
      (:prefix "t"
        :desc "command-log-mode" "c" #'global-command-log-mode))

(after! command-log-mode
  (map! :leader
        :desc "command-log-buffer" "o c" #'clm/toggle-command-log-buffer))
