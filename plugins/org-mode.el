
;; setting global key for org-mode
1(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; set default priority to C
(setq org-default-priority ?C
      org-highest-priority ?A
      org-lowest-priority ?C )

;;GTD from Nicolas Petton [https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html]

;; add the following files to agenda list
(setq org-agenda-files '("~/local_drive/personal/gtd/inbox.org"
                         "~/local_drive/personal/gtd/gtd.org"
                         "~/local_drive/personal/gtd/tickler.org"))

(setq org-refile-targets '(("~/local_drive/personal/gtd/gtd.org" :maxlevel . 3)
                           ("~/local_drive/personal/gtd/someday.org" :level . 1)
                           ("~/local_drive/personal/gtd/tickler.org" :maxlevel . 2)))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/local_drive/personal/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                               ("T" "Tickler" entry
                               (file+headline "~/local_drive/personal/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))

;; Following the GTD principle, what I usually want is to only show the first action to be done (or next action) for
;;           each project with the @office tag.
(setq org-agenda-custom-commands
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ("h" "At home" tags-todo "@home"
         ((org-agenda-overriding-header "Home")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
        ;; filter based on priority
        ("p" . "Priorities")
        ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
        ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
        ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
       ))


(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))
