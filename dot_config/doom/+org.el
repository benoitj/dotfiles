;;; ~/src/dotfiles/emacs/.config/doom/+org.el -*- lexical-binding: t; -*-


(setq org-directory "~/src/tasks")
(setq org-para-todo (concat org-directory "/todo.org"))
(setq org-agenda-files (list org-para-todo))
(setq org-refile-allow-creating-parent-nodes t)
(setq org-return-follows-link t)

(after! org
  (setq org-capture-templates
        '(
          ("t" "Task" entry (file+headline org-para-todo "Inbox")
           "* TODO %?\n")
          ("p" "Project" entry (file+headline org-para-todo "Projects")
           (file "~/.config/emacs/templates/newprojecttemplate.org"))
          ("s" "Someday" entry (file+headline "~/src/tasks/someday.org" "Someday")
           "* SOMEDAY %?\n")
          ("d" "Review: Daily Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (file "~/.config/emacs/templates/dailyreviewtemplate.org"))
          ("w" "Review: Weekly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (file "~/.config/emacs/templates/weeklyreviewtemplate.org"))
          ("m" "Review: Monthly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (file "~/.config/emacs/templates/monthlyreviewtemplate.org"))
          ("y" "Review: Yearly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (file "~/.config/emacs/templates/yearlyreviewtemplate.org"))
          )
        )

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )



(use-package! org-randomnote
  :after org
  :init
  (setq org-randomnote-candidates org-agenda-files)
  :config
  (map! :leader
        :desc "Random Note" "n R" #'org-randomnote))


(use-package! ox-reveal
  :after org
  :init
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.9.2/")
  )

;;; TODO below needs review and incorporate in review templates
(defun go-to-projects ()
  (interactive)
  (find-file "~/src/tasks/todo.org")
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
    (find-file "~/src/tasks/todo.org")
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
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
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
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
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
  (let ((org-capture-templates '(("m" "Review: Monthly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
                                  (file "~/.emacs.d/config/templates/monthlyreviewtemplate.org")))))
    (progn
      (org-capture nil "m")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))
