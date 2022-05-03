;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Benoit Joly"
      user-mail-address "benoit@benoitj.ca")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(setq doom-font (font-spec :name "FiraCode Nerd Font-14:style=Retina"))
(setq doom-variable-pitch-font (font-spec :family "Cantarell" :size 14))
(setq doom-font-increment 1)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(doom/set-frame-opacity 90)
(setq doom-theme 'modus-vivendi)
(use-package! modus-themes
  :bind ("<f5>" . modus-themes-toggle))


(setq doom-modeline-modal-icon nil) ;; turn off modeline modal state icons

(setq visible-bell t)

;; (column-number-mode) already enabled by doom
;; (global-display-line-numbers-mode t) done a different way
;; (global-hl-line-mode) doom enables

;;(set-fringe-mode 10)        ; Give some breathing room

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;(setq display-line-numbers-type t)

(when (featurep! :ui hydra)
  (map! :leader
        (:prefix "t"
         :desc "zoom" "z" #'+hydra/text-zoom/body)))

(after! evil
  (setq evil-visualstar/persistent t)
  ;; TODO (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; TODO (evil-set-initial-state 'dashboard-mode 'normal))
  )

(use-package! winner
  :after drag-stuff
  :init
  (map! "<M-left>" #'winner-undo
        "<M-right>" #'winner-redo))

(when (featurep! :ui window-select)
  (use-package! ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?h ?t ?n ?s))
  :init
  (map! :leader
        (:prefix "w"
         :desc "ace-window" "w" #'ace-window)))
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

(when (featurep! :completion company)
  (use-package! company
    :bind*
    (("M-TAB" . company-manual-begin)))
  (use-package! company-flx
    :after company
    :config
    (company-flx-mode +1))

  ;; quickhelp popup like with autocomplete
  (use-package company-quickhelp
    :after company
    :config
    (setq company-quickhelp-delay 3)
    :commands (company-quickhelp-mode)
    :init
    (company-quickhelp-mode nil)))

;;;** fix up/down case word by going to the beginning of the word
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

;; TODO popper?
;;(use-package popper
;;  :defer nil
;;  :general
;;  (bj-leader-keys
;;    "."   '(popper-toggle-latest :which-key "popup toggle")
;;    "bp"  '(:ignore t :which-key "popup")
;;    "bpl" '(popper-toggle-latest :which-key "latest")
;;    "bpp" '(popper-cycle :which-key "cycle") ;; NOTE: k before M-n kills the popup
;;    "bpt" '(popper-toggle-type :which-key "toggle type"))
;;  :init
;;  (setq popper-reference-buffers
;;        '("\\*Messages\\*"
;;          "Output\\*$"
;;	  "*YASnippet Tables*"
;;          "\\*Async Shell Command\\*"
;;          help-mode
;;          compilation-mode))
;;  (popper-mode +1)
;;  (popper-echo-mode +1))
;; TODO window maximize does not toggle
;; TODO: avy navigation. see karthinks blog

(use-package! dired
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

;; TODO dired+?

(auto-save-visited-mode 1)
(global-auto-revert-mode 1)
(setq read-file-name-completion-ignore-case t)

(map! :leader
        (:prefix "c"
         (:prefix-map ("S" . "snippets")
          :desc "insert" "i" #'yas-insert-snippet
          :desc "list"   "l" #'yas-describe-tables
          :desc "new"    "n" #'yas-new-snippet
          :desc "visit"  "v" #'yas-visit-snippet-file)))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/src/private/todos")
(setq org-agenda-files `(,(expand-file-name "todo.org" org-directory)))
(setq org-refile-targets `((,(expand-file-name "todo.org" org-directory) :maxlevel . 3)
                      (,(expand-file-name "somedaymaybe.org" org-directory) :level . 1)))
(setq org-refile-allow-creating-parent-nodes t)
(setq org-return-follows-link t)

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-appear-autolinks t)
  (setq org-appear-delay 0.5))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (plantuml . t))))

(after! org
  (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
    (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
    (add-to-list 'org-structure-template-alist '("p" . "src plantuml :file "))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
    (add-to-list 'org-structure-template-alist '("json" . "src json")))

(after! org-superstar
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq inhibit-compacting-font-caches t))

(after! org-roam
    (setq org-roam-directory (expand-file-name "~/src/projects/notebook/"))
    (defun bmj-org-id-update-org-roam-files ()
      "Update Org-ID locations for all Org-roam files."
      (interactive)
      (org-id-update-id-locations (org-roam-list-files)))

    (defun bmj-org-id-update-id-current-file ()
      "Scan the current buffer for Org-ID locations and update them."
      (interactive)
      (org-id-update-id-locations (list (buffer-file-name (current-buffer))))))

;;(use-package meme
;;  :straight
;;  (meme :host github
;;	 :repo "larsmagne/meme"
;;	 :files ("*.el" "images"))
;;  :general
;;  (bj-leader-keys
;;    "oM"  '(:ignore t :which-key "meme")
;;    "oMM"  '(meme :which-key "meme")
;;    "oMf"  '(meme-file :which-key "meme-file")))
;;
;;(use-package imgur)
;;
;;(use-package snow
;;  :general
;;  (bj-leader-keys
;;    "as" '(snow :which-key "snow")))



;; TODO magit-annex
;; TODO git-link
;(use-package git-link
;  :general
;  (bj-leader-keys
;    "gl" '(:ignore t :which-key "links")
;    "gll" '(git-link :which-key "file")
;    "glc" '(git-link-commit :which-key "commit")
;    "glh" '(git-link-homepage :which-key "homepage")))
;;;;** pomodoro
;(use-package pomm
;  :straight (:host github :repo "SqrtMinusOne/pomm.el")
;  :commands (pomm)
;  :general
;  (bj-leader-keys
;    "tp" '(:ignore t :which-key "pommodoro")
;    "tpp" '(pomm :which-key "pomm")
;    "tpt" '(pomm-pause :which-key "toggle")
;    "tpx" '(pomm-stop :which-key "stop")
;    )
;  :config
;  (pomm-mode-line-mode t))
;;;;** jq json tool
;(use-package jq-mode)
;(use-package restclient
;  :straight
;  (restclient :type git :host github
;	      :repo "pashky/restclient.el"
;	      :files ("restclient.el" "restclient-jq.el"))
;  :defer nil
;  :mode (("\\.rest\\'" . restclient-mode))
;  :config
;  (require 'restclient-jq))
;
;(use-package restclient-test
;  :hook (restclient-mode . restclient-test-mode)
;  :general
;  (bj-local-leader-keys
;   :states '(normal insert)
;   :keymaps 'restclient-mode-map
;   "t" '(:ignore t :which-key "test")
;   "tt" '(restclient-test-buffer :which-key "buffer")
;   "tc" '(restclient-test-current :which-key "current")))
;
;;;;** pastebin 0x0
;(use-package 0x0
;  :straight (0x0 :host gitlab :repo "willvaughn/emacs-0x0")
;  :general
;  (bj-leader-keys
;    "bu" '(:ignore t :which-key "upload")
;    "buf" '(0x0-upload-file :which-key "file")
;    "bup" '(0x0-popup :which-key "popup")
;    "but" '(0x0-upload-text :which-key "text")))
;;;** restclient

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(use-package! ement
  :commands ement-connect)

(setq! ement-save-sessions t
       ement-room-send-message-filter #'ement-room-send-org-filter
       ement-sessions-file (concat doom-cache-dir "ement.el"))

(after! ement
  (add-hook! 'doom-real-buffer-functions
    (defun +ement-buffer-p (buf)
      (string-prefix-p "ement-" (symbol-name (buffer-local-value 'major-mode buf)))))
  (add-hook! 'ement-room-mode-hook (setq-local fringes-outside-margins nil)))

(set-popup-rule! "^\\*Ement compose:" :side 'bottom :size 0.2 :quit 'current)

(custom-theme-set-faces
 'user
 '(ement-room-message-text ((t (:inherit variable-pitch))))
 '(ement-room-self-message ((t (:inherit variable-pitch)))))
