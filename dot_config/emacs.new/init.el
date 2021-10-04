;; init.el --- my new emacs-lisp configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author:  Benoit Joly <benoit@benoitj.ca>
;; Keywords:
;; Package-Requires: ((emacs "28.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the nth version of my config. This time, I'm back to elisp
;; configuration.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; garbage collection tuning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst bj-100MB (* 100 1000 1000))
(defconst bj-20MB (* 20 1000 1000))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold bj-100MB)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold bj-20MB)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; monitor performance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bj-display-startup-time ()
  "Displays time to load and gc metrics."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'bj-display-startup-time)

(message "test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package management setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keep things clean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :demand t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil
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
   "f" '(:ignore t :which-key "files")
   "ff" '(find-file :which-key "open")
   "fs" '(save-buffer :which-key "save")
   "fd" '(:ignore t :which-key "dotfiles")
   "fdd" '(bj/open-dotfiles :which-key "dotfiles")
   "fde" '(bj/open-dot-emacs :which-key "emacs")
   "h" '(:keymap help-map :which-key "help")
   "m" '(:ignore t :which-key "mode")
   "q" '(:ignore t :which-key "quit")
   "qq" '(kill-emacs :which-key "kill emacs")
   "t"  '(:ignore t :which-key "toggles")
   "w" '(:ignore t :which-key "windows")
   "ww" '(other-window :which-key "switch")
   "wd" '(delete-window :which-key "delete")
   "wo" '(delete-other-windows :which-key "delete others")
   "ws" '(split-window-below :which-key "split horiz")
   "wv" '(split-window-right :which-key "split vert")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window/buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling and navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-conservatively 101)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package "use-package")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
