;;; DONE
(use-package counsel-projectile
  :bind (("C-x p" . 'projectile-command-map)))
  :config
  (counsel-projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (ryo-modal-keys
   ("SPC p" (
   	     ("p" projectile-switch-project)
   	     ("f" projectile-find-file)
   	     ("g" counsel-projectile-git-grep)
   	     ("a" projectile-ag)
   	     )))
