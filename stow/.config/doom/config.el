;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(let ((config-local-file (expand-file-name "~/.config/emacs/local.el")))
  (when (file-exists-p config-local-file)
    (load-file config-local-file)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code Retina" :size 14)
      doom-big-font (font-spec :family "Fira Code Retina" :size 36)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 14))

(use-package! notmuch
  :config
  (set-popup-rule! "^\\*notmuch" :ignore t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq confirm-kill-emacs nil)
(setq
 evil-vsplit-window-right t
 evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-vivendi)
(use-package! modus-themes
    :bind ("<f5>" . modus-themes-toggle))

(use-package! doom-modeline
;;       :custom-face
;;       (mode-line ((t (:height 0.85))))
;;       (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-modal-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-state-icon t))

(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq display-line-numbers-type t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/src/notebook/")

(setq ivy-use-selectable-prompt t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Email / notmuch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 user-full-name "Benoit Joly"
 user-mail-address "benoit@benoitj.ca"
 user-mail-addresses '("benoit@benoitj.ca" "benoit.m.joly@gmail.com" "bjoly666@gmail.com"))

(after! notmuch
  (map! :map notmuch-show-mode-map :localleader :desc "browse urls" "b" #'notmuch-show-browse-urls))

(after! ace-window
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?s ?/ ? l)
        aw-scope 'frame
        aw-background t))

;; use ace-window instead of evil-window-next
(map! :leader :desc "ace-window" "w w" #'ace-window)
