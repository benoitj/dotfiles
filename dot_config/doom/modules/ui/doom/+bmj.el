;;; ui/doom/+bmj.el -*- lexical-binding: t; -*-
;;;###if (featurep! +bmj)

(when (featurep! :ui doom +bmj)
  (message "loading my custom themes")
  (setq doom-theme 'modus-vivendi)
  (setq display-line-numbers-type t))
