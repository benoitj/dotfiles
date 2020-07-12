;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; load local file that has user/system specific information
(let ((config-local-file (expand-file-name "~/.config/emacs/local.el")))
  (when (file-exists-p config-local-file)
    (load-file config-local-file)
	 ))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

(setq ivy-use-selectable-prompt t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(load! "+completion.el")
(load! "+ui.el")
(load! "+images.el")
(load! "+org.el")
(load! "+org-notes.el")
(load! "+mu4e.el")
(load! "+vc.el")
(load! "+term.el")
(load! "+log.el")
(load! "+social.el")
(load! "+lang-adoc")
