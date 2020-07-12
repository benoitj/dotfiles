;;; ~/src/dotfiles/emacs/.config/doom/+ui.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :name "Hack" :size 14))

(use-package! heaven-and-hell
    :init
    (setq heaven-and-hell-theme-type 'dark)
    (setq heaven-and-hell-themes
	  '((light . espresso)
	    (dark . dracula)))
    (setq heaven-and-hell-load-theme-no-confirm t)
    :bind (("<f6>" . 'heaven-and-hell-toggle-theme)))

(setq doom-theme 'dracula)

;;
;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

(setq confirm-kill-emacs nil)

(add-hook! 'prog-mode-hook #'rainbow-delimiters-mode)

  ;;(global-set-key [remap other-window] #'ace-window)
(after! ace-window
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?s ?/ ? l)
        aw-scope 'frame
        aw-background t)
  )

(map! :leader :desc "ace-window" "w w" #'ace-window)
(map! :leader :desc "text-zoom" "w z" #'+hydra/text-zoom/body)
