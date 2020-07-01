;;; -*- lexical-binding: t -*-
;;; DONE
(use-package helm)

;; counsel replaces ido matcher
  (use-package counsel
    :config
    (counsel-mode 1)
    (global-set-key (kbd "C-x f") 'counsel-recentf)
      (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
      (global-set-key (kbd "C-c g") 'counsel-git)
  ;;    (global-set-key (kbd "C-c j") 'counsel-git-grep)
      (global-set-key (kbd "C-c k") 'counsel-ag)
  ;;    (global-set-key (kbd "C-x l") 'counsel-locate)
  ;;    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;;    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    )

  ;; general purpose matcher
  (use-package ivy
    :diminish (ivy-mode)
    :bind (("C-c C-r" . 'ivy-resume))
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ) 

  ;; search matcher replacement
  (use-package swiper
    :bind (("\C-s" . 'swiper))
    )

  ;; avy quick char navigation. many more avy-goto-* functions
  (use-package avy
    :bind ("M-s" . 'avy-goto-char))

  (use-package evil-visualstar
    :after evil
    :config
    (global-evil-visualstar-mode t))

  (use-package ag)

  (use-package deadgrep
  :bind (("C-c d" . deadgrep)
         ("C-c D" . counsel-rg)
         (:map deadgrep-mode-map
               ("q" . kill-this-buffer))))
