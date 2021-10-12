;;; -*- lexical-binding: t; -*-

;;; * garbage collection tuning
(defconst bj-100MB (* 100 1000 1000))
(defconst bj-20MB (* 20 1000 1000))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold bj-100MB)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold bj-20MB)))

;;; * move custom config to a file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; * monitor performance
(defun bj-display-startup-time ()
  "Displays time to load and gc metrics."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%s" (emacs-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'bj-display-startup-time)

;;; * package management setup
(require 'package)
(cl-pushnew '("melpa" . "https://melpa.org/packages/") package-archives :test #'equal)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package use-package
  :custom
  (use-package-always-defer t)
  (use-package-always-ensure nil)
  (use-package-verbose t)
  (straight-use-package-by-default t))

;;; * UI configuration
;;; ** Basic UI configuration
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
;; TODO: split by modes using use-package
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;;; ** Theme
(use-package modus-themes
  :demand t
  :init
  ;; Add all your customizations prior to loading the themes
  ;;(setq modus-themes-slanted-constructs t
  ;;     modus-themes-bold-constructs nil)

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

;;; ** Fonts
(defmacro bj-run-now-or-on-make-frame-hook (&rest body)
  "Macro created to run now on setup hooks when running as a daemon.
BODY is the symbol or expression to run."
  `(if (daemonp)
       (add-hook 'server-after-make-frame-hook (lambda () ,@body))
     (progn ,@body)))

(setq bj-default-font-size 120)
(setq bj-fixed-font-name "Fira Code Retina")
(setq bj-variable-font-name "Cantarell")

(setq bj-frame-transparency '(90 . 90))
(set-frame-parameter (selected-frame) 'alpha bj-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,bj-frame-transparency))
;; TODO: seem overly complex
(defcustom
  bj-font-size bj-default-font-size "My default font size.")

(defun bj-set-frame-font-size (&optional font-size)
  "change frame font size to font-size.
      If no font-size specified, reset to default."
  (let ((font-size
         (or font-size
             (car (get 'bj-font-size 'standard-value)))))
    (customize-set-variable 'bj-font-size font-size)
    (set-face-attribute 'default nil :font bj-fixed-font-name :height font-size)

    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font bj-fixed-font-name :height font-size)

    (set-face-attribute 'variable-pitch nil :font bj-variable-font-name :height font-size :weight 'regular)))

(defun bj-increase-frame-font ()
  "Increase font by 1"
  (interactive)
  (bj-set-frame-font-size (+ bj-font-size 10)))

(defun bj-decrease-frame-font ()
  "Decrease font by 1"
  (interactive)
  (bj-set-frame-font-size (- bj-font-size 10)))

(defun bj-reset-frame-font ()
  "Reset font size to default"
  (interactive)
  (bj-set-frame-font-size bj-default-font-size))

(with-eval-after-load 'hydra
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("+" bj-increase-frame-font "in")
    ("-" bj-decrease-frame-font "out")
    ("0" bj-reset-frame-font "reset")
    ("q" nil "finished" :exit t))

  (bj-leader-keys
   "ts" '(hydra-text-scale/body :which-key "scale text")))

(bj-run-now-or-on-make-frame-hook (bj-reset-frame-font))

;;; * keep things clean
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :demand t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; * history and undo
;; History
(setq savehist-file "~/.config/emacs.new/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring
	search-ring
  	regexp-search-ring)
      recentf-max-saved-items 50)

(savehist-mode 1)
(recentf-mode 1)

;;; * Bindings
;;; ** evil
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; TODO Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :demand t
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode t))

;; General <space> configuration
(defun bj-open-dot-emacs ()
  "Open Emacs init.el."
  (interactive)
  (find-file (expand-file-name "~/src/projects/dotfiles/dot_config/emacs.new/init.el")))

(defun bj-open-dotfiles ()
  "Open dotfiles in dired."
  (interactive)
  (find-file (expand-file-name "~/src/projects/dotfiles/")))

;;; ** general
(use-package general
  :after evil
  :config
  (general-create-definer bj-leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer bj-local-leader-keys
    :prefix "SPC m"
    :global-prefix "M-SPC m")

  (bj-local-leader-keys
      :states '(normal insert)
      :keymaps 'org-mode-map
      "t" '(org-todo :which-key "toggle todo"))

  (bj-leader-keys
   "b" '(:ignore t :which-key "buffers")
   "bb" '(switch-to-buffer :which-key "buffers")
   "bi" '(ibuffer :which-key "ibuffer")
   "bd" '(kill-current-buffer :which-key "kill current")
   "bD" '(kill-buffer :which-key "kill")
   "br" '(revert-buffer :which-key "revert")
   "f" '(:ignore t :which-key "files")
   "ff" '(find-file :which-key "open")
   "fs" '(save-buffer :which-key "save")
;;TODO:   "fd" '(:ignore t :which-key "dotfiles")
   "fr" '(recentf-open-files :which-key "recent")
   "fdd" '(bj-open-dotfiles :which-key "dotfiles")
   "fde" '(bj-open-dot-emacs :which-key "emacs")
   "h" '(:keymap help-map :which-key "help")
   "ha" '(apropos :which-key "apropos")
   "hM" '(man :which-key "man")
   "m" '(:ignore t :which-key "mode")
   "n" '(:ignore t :which-key "navigate")
   "ni" '(imenu :which-key "imenu")
   "q" '(:ignore t :which-key "quit")
   "qq" '(kill-emacs :which-key "kill emacs")
   "s"  '(:ignore t :which-key "search")
   "sl" '(locate :which-key "locate")
   "t"  '(:ignore t :which-key "toggles")
   "w" '(:ignore t :which-key "windows")
   "ww" '(other-window :which-key "switch")
   "wd" '(delete-window :which-key "delete")
   "wo" '(delete-other-windows :which-key "delete others")
   "ws" '(split-window-below :which-key "split horiz")
   "wv" '(split-window-right :which-key "split vert")))

;;; ** Hydras
(use-package hydra)


;;; * window/buffer management
(use-package ace-window
  :demand t
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?h ?t ?n ?s))
  :init
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))
;; TODO: setup contextual buffer list (? bufler)
;; TODO: various window/frame setup (?perspective or tab-bar)
;; TODO: save and restore buffers and workspace (?burly)
;; (desktop-save-mode t)
;; (add-hook 'desktop-after-read-hook 'bj-reset-theme-hook)
;; (save-place-mode t)
;; TODO: popup management (?popper)
;; TODO: window placement

;;; * scrolling and navigation
(setq scroll-conservatively 101)

;;; * searching
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

;;; * project/file management
(use-package project
  :general
  (bj-leader-keys
    "p" '(:ignore t :which-key "project")
    "pa" '(project-async-shell-command :which-key "async cmd")
    "pb" '(project-switch-to-buffer :which-key "buffers")
    "pC" '(project-shell-command :which-key "command")
    "pc" '(project-compile :which-key "compile")
    "pd" '(project-dired :which-key "dired")
    "pf" '(project-find-file :which-key "find file")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "pp" '(project-switch-project :which-key "switch")
    "pr" '(project-query-replace-regexp :which-key "replace")
    "pt" '(project-shell :which-key "terminal")
    ;; replaced by deadgrep due to editable buffer not working
    ;; "sp" '(project-find-regexp :which "regexp search")
    "<SPC>" '(project-find-file :which-key "project files"))
  :config
  (cl-defgeneric project-root (project)
    "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute."
      (car project))
  (eval-after-load 'consult
     (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))))

;;; * vertical completion
(use-package vertico
  ;; TODO: can we defer until first input?
  :demand t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)
  ) 

(use-package orderless
  :after vertico
  :custom (completion-styles '(orderless)))

(use-package consult
  :general
  (general-define-key
   [remap apropos]                       #'consult-apropos
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-jumps]               #'+vertico/jump-list
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   )
  (bj-leader-keys
    "ss" '(consult-ripgrep :which-key "ripgrep")))

;; TODO: https://karthinks.com/software/fifteen-ways-to-use-embark/

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)     
   ("C-," . embark-export)   
   ("C-;" . embark-dwim)   
   ("C-h B" . embark-bindings))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable richer annotations like ivy-helpful
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;;; * company

(use-package company
  :demand t
;;  :hook (after-init . global-company-mode)
  :custom
  (company-require-match #'company-explicit-action-p)
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.2)
  (company-tooltip-align-annotation t)
  (company-auto-complete-chars nil)
  (company-frontends '(company-pseudo-tooltip-frontend
		       company-echo-metadata-frontend))
  :bind
  (([remap completion-at-point]  . company-manual-begin)
   ([remap completion-symbol]  . company-manual-begin)  
   
   :map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("<tab>" . company-complete-selection)
   ("TAB" . company-complete-selection)
   ("SPC" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   :map company-active-map
   :filter (company-explicit-action-p)
   ("<return>" . company-complete-selection)
   ("RET"  . company-complete-selection))

  :bind*
  (("M-TAB" . company-manual-begin))
  :config
  (global-company-mode))

;; provide partial matches in completion like with intellij
(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

;;;; quickhelp popup like with autocomplete
(use-package company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay 3)
  :commands (company-quickhelp-mode)
  :init
  (company-quickhelp-mode nil))

; TODO: what is it used for
(use-package pos-tip
    :commands (pos-tip-show))

 ;; FIXME: somehow company-box does not have proper icons loaded
;(use-package company-box
;  :after (company all-the-icons)
;  :hook (company-mode . company-box-mode))

;;; * VC
(use-package magit
  :general
  (bj-leader-keys
    "g" '(:ignore t :which-key "git")
    "gd" '(magit-file-dispatch :which-key "file dispatch")
    "gg" '(magit-status :which-key "status")))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package git-link
  :general
  (bj-leader-keys
    "gl" '(:ignore t :which-key "links")
    "gll" '(git-link :which-key "file")
    "glc" '(git-link-commit :which-key "commit")
    "glh" '(git-link-homepage :which-key "homepage")))

;;; * Org
(defun bj-find-in-notes ()
  "Find a file under `org-directory'"
  (interactive)
  (unless (bound-and-true-p org-directory)
    (require 'org))
  (consult-find org-directory))

(use-package org
  :general
  (bj-leader-keys
    "n" '(:ignore t :which-key "notes")
    "na" '(org-agenda :which-key "agenda")
    "nn" '(org-capture :which-key "capture")
    "nf" '(bj-find-in-notes :which-key "find notes"))
    
  (bj-local-leader-keys
   :states '(normal insert)
   :keymaps 'org-mode-map
   "t" '(org-todo :which-key "toggle todo"))
  :custom
  (org-directory "~/src/private/todos"))

;; TODO: org-noter?


;;; * terminal
(use-package vterm
  :general
  (bj-leader-keys
    "ot"  '(:ignore t :which-key "term")
    "ott" '(vterm :which-key "vterm")))

(use-package multi-vterm
  :general
  (bj-leader-keys
    "pt"  '(multi-vterm-project :which-key "vterm")))

;;; * Editor
;; fix up/down case word by going to the beginning of the word
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (looking-back "\\b" nil)
    (backward-word)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-color-background t) 
  :custom-face
  (hl-todo ((t (:bold t :foreground "#111111"))))
  :general
  (bj-leader-keys
    "nt"  '(:ignore t :which-key "todos")
    "ntn"  '(hl-todo-next :which-key "next")
    "ntp"  '(hl-todo-previous :which-key "prev")
    "st" '(hl-todo-occur :which-key "todos")))

(use-package undo-tree
  ;; TODO: find a better way to defer it
  :demand t
  :config
  (global-undo-tree-mode))

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (clojure-mode . rainbow-delimiters-mode)))
   
;;; * Fun
(use-package meme
  :straight
  (meme :host github
	 :repo "larsmagne/meme"
	 :files ("*.el" "images"))
  :general
  (bj-leader-keys
    "oM"  '(:ignore t :which-key "meme")
    "oMM"  '(meme :which-key "meme")
    "oMf"  '(meme-file :which-key "meme-file")))

(use-package imgur)

;;; * LOCAL-VARIABLES
;; Local Variables:
;; outline-regexp: ";;; \\*+"
;; page-delimiter: ";;; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-sublevels 5)
;; End:
