;;; tools/command-log/config.el -*- lexical-binding: t; -*-

(use-package! command-log-mode
  :commands (command-log-mode global-command-log-mode)
  )

;;(map! :leader
;;      (:prefix "t"
;;        :desc "command-log-mode" "c" #'global-command-log-mode))
;;
;;(after! command-log-mode
;;  (map! :leader
;;        :desc "command-log-buffer" "o c" #'clm/toggle-command-log-buffer))
