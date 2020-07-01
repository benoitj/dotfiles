;;; -*- lexical-binding: t -*-
;; DONE
;; org-mode is loaded in init.el due to race conditions which
;; could load the emacs embedded org-mode instead of
;; latest version loaded by straight

(use-package helm-org-rifle
  :bind (("C-c s" . 'helm-org-rifle-org-directory))
  )
(setq org-directory "~/src/notes")

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   (shell . t))) 

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

  (add-to-list
   'org-modules 'org-tempo)

(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(use-package ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(setq org-agenda-files '("~/src/notes/todo.org"))


(use-package org-randomnote
  :bind (:map launcher-map
              ("r" . org-randomnote))
  :init
  (setq org-randomnote-candidates '("~/src/notes/todo.org")))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/src/notes/todo.org" "Inbox")
         "* TODO %?\n")
        ("p" "Project" entry (file+headline "~/src/notes/todo.org" "Projects")
         (file "~/.emacs.d/config/templates/newprojecttemplate.org"))
        ("s" "Someday" entry (file+headline "~/src/notes/someday.org" "Someday / Maybe")
         "* SOMEDAY %?\n")
        ("m" "Maybe" entry (file+headline "~/src/notes/someday.org" "Someday / Maybe")
         "* MAYBE %?\n")
        ("l" "Log" entry (file+olp+datetree "~/src/notes/log.org" "Log")
         (file "~/.emacs.d/config/templates/logtemplate.org"))))


(defun go-to-projects ()
  (interactive)
  (find-file "~/src/notes/todo.org")
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* Projects")
  (beginning-of-line))

(defun project-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?p)
  (org-columns))

(defun project-deadline-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?d)
  (org-columns))

(defun my-org-agenda-list-stuck-projects ()
  (interactive)
  (go-to-projects)
  (org-agenda nil "#" 'subtree))

(defun go-to-areas ()
    (interactive)
    (find-file "~/src/notes/todo.org")
    (widen)
    (beginning-of-buffer)
    (re-search-forward "* Areas")
    (beginning-of-line))

(defun areas-overview ()
    (interactive)
    (go-to-areas)
    (org-narrow-to-subtree)
    (org-columns))

(defun my-new-daily-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "~/src/notes/reviews.org")
                                  (file "~/.emacs.d/config/templates/dailyreviewtemplate.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      ;(fetch-calendar)
      (org-clock-in))))

(defun my-new-weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree "~/src/notes/reviews.org")
                                  (file "~/.emacs.d/config/templates/weeklyreviewtemplate.org")))))
    (progn
      (org-capture nil "w")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

(defun my-new-monthly-review ()
  (interactive)
  (let ((org-capture-templates '(("m" "Review: Monthly Review" entry (file+olp+datetree "~/src/notes/reviews.org")
                                  (file "~/.emacs.d/config/templates/monthlyreviewtemplate.org")))))
    (progn
      (org-capture nil "m")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

(bind-keys :prefix-map review-map
           :prefix "C-c r"
           ("d" . my-new-daily-review)
           ("w" . my-new-weekly-review)
           ("m" . my-new-monthly-review))

(use-package f)
(f-touch "~/src/notes/reviews.org")

(setq org-refile-targets '(("~/src/notes/todo.org" :maxlevel . 3)
			   ("~/src/notes/somedaymaybe.org" :level . 1)))
      
(setq org-refile-allow-creating-parent-nodes t)
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-return-follows-link t)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


;; ;(server-start)
;; ;(require 'org-protocol)

;; (use-package org-web-tools)

;; (use-package s)
;; ;(use-package org-protocol-capture-html
;; ;  :straight (el-patch :type git :host github :repo "alphapapa/org-protocol-capture-html"))


