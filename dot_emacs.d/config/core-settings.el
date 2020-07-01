;;; DONE


(defun bj/reset-theme-hook ()
  "hack that toggle twice theme to get proper syntax highlighting"
  (interactive)
  (heaven-and-hell-toggle-theme)
  (heaven-and-hell-toggle-theme)
  )


;; setup UI
(defun bj/config-ui-tweaks ()
  ""

  (setq ring-bell-function 'ignore
	x-gtk-use-system-tooltips nil
	use-dialog-box nil
	inhibit-startup-message t)


  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq confirm-kill-processes nil)

  ;; visual attributes
  (global-hl-line-mode 1)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  
  (desktop-save-mode t)
  (add-hook 'desktop-after-read-hook 'bj/reset-theme-hook)
  (save-place-mode t)

  (use-package which-key
    :ensure t
    :config (which-key-mode))

  )

(defun bj/config-images ()

  (use-package all-the-icons
    :ensure t)

  (use-package emojify
    :ensure t)
)


(defun bj/config-theme ()
  ""
  ;; fonts
  (setq benoitj/font-name "Hack")
  (defcustom benoitj/font-size 12 "My default font size")

  ;; themes
  (use-package dracula-theme
    :ensure t)

  (use-package espresso-theme
    :ensure t)

  (use-package heaven-and-hell
    :ensure t
    :init
    (setq heaven-and-hell-theme-type 'dark)
    (setq heaven-and-hell-themes
	  '((light . espresso)
	    (dark . dracula)))
    (setq heaven-and-hell-load-theme-no-confirm t)
    (add-hook 'after-init-hook 'heaven-and-hell-init-hook)    
    :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
	   ("<f6>" . heaven-and-hell-toggle-theme)))
  
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-init)
    :config
    (setq doom-modeline-icon t))		

  ;; coloring of delimiters
  (use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
  
  )




(defun set-frame-font-size (&optional font-size)
  "change frame font size to font-size.
If no font-size specified, reset to default."
  (let ((font-size
	 (or font-size   
	     (car (get 'benoitj/font-size 'standard-value)))))
    (customize-set-variable 'benoitj/font-size font-size)
    (set-frame-font
     (format "%s %d" benoitj/font-name font-size) nil t)))

(defun increase-frame-font ()
  "Increase font by 1"
  (interactive)
  (set-frame-font-size (+ benoitj/font-size 1)))

(defun decrease-frame-font ()
  "Decrease font by 1"
  (interactive)
  (set-frame-font-size (- benoitj/font-size 1)))

(defun reset-frame-font ()
  "Reset font size to default"
  (interactive)
  (set-frame-font-size ))

(add-hook 'after-init-hook 'reset-frame-font)

(global-set-key (kbd "C-x C-0") 'reset-frame-font)
(global-set-key (kbd "C-x C--") 'decrease-frame-font)
(global-set-key (kbd "C-x C-=") 'increase-frame-font)
(global-set-key (kbd "C-x C-+") 'text-scale-adjust)


(bj/config-ui-tweaks)
(bj/config-theme)
(bj/config-images)
