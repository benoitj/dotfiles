;;; ~/src/dotfiles/emacs/.config/doom/+vc.el -*- lexical-binding: t; -*-

(setq projectile-project-search-path '("/home/benoit/src"))

(use-package! git-link
  :commands
  (git-link git-link-commit git-link-open-in-browser)
  :init (setq git-link-open-in-browser t))

(after! magit
  :config
 (setq magit-diff-refine-hunk 'all))

 ;;(defun wg/kludge-gpg-agent
 ;;      ()
 ;;        (if
 ;;	        (display-graphic-p)
 ;;		      (setenv "DISPLAY"
 ;;			                    (terminal-name))
 ;;		          (setenv "GPG_TTY"
 ;;				              (terminal-name))
 ;;			      (setenv "DISPLAY")))
 ;;
 ;;(add-hook 'window-configuration-change-hook 'wg/kludge-gpg-agent)
