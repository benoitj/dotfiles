;;; tools/command-log/config.el -*- lexical-binding: t; -*-

(use-package! command-log-mode
  :commands (command-log-mode global-command-log-mode))

(map! :leader
      (:prefix "t"
       :desc "command-log-mode" "C" #'global-command-log-mode))

(map! :leader
      (:prefix o
      :desc "command-log-buffer" "c" #'clm/toggle-command-log-buffer))
