;;; ~/src/dotfiles/emacs/.config/doom/+org.el -*- lexical-binding: t; -*-

(setq plantuml-default-exec-mode 'jar)
(setq plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar")

(setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc")

(setq org-directory "~/src/notebook")
(setq org-para-todo (concat org-directory "/todo.org"))
(setq org-templates-directory (concat org-directory "/templates/"))
(setq org-agenda-files (list org-para-todo))
(setq org-refile-allow-creating-parent-nodes t)
(setq org-return-follows-link t)
(setq org-image-actual-width "200px")

(use-package! deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/src/notebook"))

(use-package! org-bom
  :after org
  :ensure t)

(defun bj/read-template (template)
  ""
  (with-temp-buffer
    (insert-file-contents (concat org-templates-directory template))
    (buffer-string)))

(defun bj/read-newproject-template ()
  ""
  (bj/read-template "newprojecttemplate.org"))

(defun bj/read-dailyreview-template ()
  ""
  (bj/read-template "dailyreviewtemplate.org"))

(defun bj/read-weeklyreview-template ()
  ""
  (bj/read-template "weeklyreviewtemplate.org"))

(defun bj/read-monthlyreview-template ()
  ""
  (bj/read-template "monthlyreviewtemplate.org"))

(defun bj/read-yearlyreview-template ()
  ""
  (bj/read-template "yearlyreviewtemplate.org"))

(defun bj/create-area-note ()
  ""
  org-para-todo)

(after! org
  (setq org-capture-templates
        '(
          ("t" "Task" entry (file+headline org-para-todo "Inbox")
           "* TODO %?\n")
          ("p" "Project" entry (file+headline org-para-todo "Projects")
           (function bj/read-newproject-template))
          ("s" "Someday" entry (file+headline (concat org-directory "/someday.org") "Someday")
           "* SOMEDAY %?\n")
          ("d" "Review: Daily Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (function bj/read-dailyreview-template))
          ("w" "Review: Weekly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (function bj/read-weeklyreview-template))
          ("m" "Review: Monthly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (function bj/read-monthlyreview-template))
          ("y" "Review: Yearly Review" entry (file+olp+datetree "~/src/tasks/reviews.org")
           (function bj/read-yearlyreview-template))
          )
        )

  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )

(use-package! ox-rss
  :after org)

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
