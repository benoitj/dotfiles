;;; -*- lexical-binding: t -*-
;;; DONE
(use-package hydra)

;; window history using c-c <- and ->
(winner-mode 1)

;; select windows using c-x o <a number>

(use-package ace-window
  :init
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0)))))
  )
  
(use-package smooth-scrolling
   :config
   (smooth-scrolling-mode 1))

(setq scroll-conservatively 101)

;; bind ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :requires (magit)
  :bind
  ("M-SPC" . ryo-modal-mode)
  :config
  (ryo-modal-keys
   ("u" ryo-modal-mode)
   ("a" counsel-M-x)
   ("c" previous-line)
   ("t" next-line)
   ("h" backward-char)
   ("n" forward-char)
   ("g" left-word)
   ("r" right-word)
   ("d" beginning-of-line-text)
   ("s" end-of-line)
   ("-" swiper)
   ("," "C-c")
   ("x" "C-x")
   
   )

  (ryo-modal-keys
   ("SPC a" (
	     ("m" mu4e)
	     ))
   ("SPC b" (
	     ("k" kill-buffer)
	     ("b" ivy-switch-buffer)
	     ("i" ibuffer)
	     ))
  ("SPC f" (
	     ("f" counsel-find-file)
	     ("r" counsel-recentf)
	     ("d" dired)
	    ))
  ("SPC g" (
	    ("g" counsel-ag)
	    ("z" magit)
	    )) 

  ("SPC q" (
	    ("q" save-buffers-kill-emacs)
	    )) 

  ("SPC s" (
	    ("s" save-buffer)
	    ("k" revert-buffer)
	    ("a" save-some-buffers)
	    )) 

  ("SPC z" (
	    ("0" reset-frame-font)
	    ("-" decrease-frame-font)
	    ("=" increase-frame-font)
	    ("+" text-scale-adjust)
	    ))
  ))

