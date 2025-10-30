
;;archive file should be pre-pended with the year and filed under date subtree
(setq org-archive-location (concat (format-time-string "%Y-" (current-time))
                                   "%s_archive::datetree/"))

    ;; setting global key for org-mode
    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)

;; Initial visivibility set to overview
(setq org-startup-folded t)
    ;; set default priority to C
(setq org-default-priority ?C
      org-highest-priority ?A
      org-lowest-priority ?C )

;; Set the repeater task back to in TODO state
(setq org-todo-repeat-to-state "TODO")

    ;;GTD from Nicolas Petton [https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html]

    ;; add the following files to agenda list
    (setq org-agenda-files '("~/local_drive/personal/OrgNotes/gtd.org"
                             "~/local_drive/personal/OrgNotes/inbox.org"
                             "~/local_drive/personal/OrgNotes/tickler.org"
                             "~/local_drive/personal/OrgNotes/habits.org"))

    (setq org-refile-targets '(("~/local_drive/personal/OrgNotes/gtd.org" :maxlevel . 3)
                               ("~/local_drive/personal/OrgNotes/someday.org" :level . 1)
                               ("~/local_drive/personal/OrgNotes/tickler.org" :maxlevel . 2)))

    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                   (file+headline "~/local_drive/personal/OrgNotes/inbox.org" "Tasks")
                                   "* TODO %i%?")
                                   ("T" "Tickler" entry
                                   (file+headline "~/local_drive/personal/OrgNotes/tickler.org" "Tickler")
                                   "* %i%? \n %U")))

    ;; ledger variables
    ;; Credit cards
    ;;defvar cs "chase-sapphire"
    ;;defvar cc "costco"
    ;;Assett
    ;;defvar ac "chase:checking"
    ;;defvar as "chase:saving"
    ;; Ledger mode captures
    (setq org-capture-templates
          (append '(("l" "Ledger entries")
                    ("le" "Expense" plain
                     (file  "~/local_drive/personal/ledger/ledger-2022.dat")
                     "%(org-read-date) %^{Payee}
        Expenses:%^{Account}   %^{Amount}
        Liabilities:%^{Credit-Card}
    "
                     :empty-lines 1
                     :immediate-finish t)
                    ("li" "Investment" plain
                    (file  "~/local_drive/personal/ledger/ledger-2022.dat")
                    "%(org-read-date) * %^{Payee}
        Asset:%^{to_account}   %^{Amount}
        Asset:%^{from_account}
    "
                    :empty-lines 1
                    :immediate-finish t)
                    ("ls" "Salary" plain
                     (file  "~/local_drive/personal/ledger/ledger-2022.dat")
                     "%(org-read-date) * %^{Payee} USD
      Asset:Bank:Chase:%^{Account}    %^{Amount} USD
      Income:Salary
    "
                     :empty-lines 1
                     :immediate-finish t))
                  org-capture-templates))

    ;; todo->not started, Next->should be picked up next, WAITING: work is started bu not finished, SKIPPED->not finished , DONE->Finished
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "SKIPPED(s)" "DONE(d)")))

    ;; Following the GTD principle, what I usually want is to only show the first action to be done (or next action) for
    ;;           each project with the @office tag.
    (setq org-agenda-custom-commands
          '(("o" "At the office" tags-todo "@office"
             ((org-agenda-overriding-header "Office")
              (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
            ("h" "At home" tags-todo "@home"
             ((org-agenda-overriding-header "Home")
              (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
            ("t" "Talking" tags-todo "@agenda"
             ((org-agenda-overriding-header "Agenda")
              (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
            ("p" "Projects"
             ((org-agenda-overriding-header "Projects")
              (org-agenda-skip-function #'my-org-agenda-skip-all-but-project-heading)))
            ;; filter based on priority
            ("p" . "Priorities")
            ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
            ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
            ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
           ))

(load "team-plan")

(defun my/org-owner-roadmap (owner)
  "Show all entries in ~/org/roadmap.org with :OWNER: = OWNER."
  (interactive "sOwner: ")
  (let ((org-agenda-files (list "~/local_drive/personal/OrgNotes/team-work-planning.org")))
    ;; tags view with no scope string (empty) but skip non‑owners:
    (org-tags-view nil
                   (format "+OWNER=\"%s\"" owner))))

;; Example binding:
(global-set-key (kbd "C-c b") #'my/org-owner-roadmap)

;; -*- lexical-binding: t; -*-

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    ;;(unless (and (org-current-is-todo) (org-goto-first-child)) ;; if the current item does not have any childeren then don't skip this current item
    (unless (and (org-current-is-todo) (string= "Projects" (org-get-category)))
      (setq should-skip-entry t))
    (if (string= "Tasks" (org-get-category))
        (setq should-skip-entry nil))
    (save-excursion
      (while( and (and (not should-skip-entry) (org-goto-sibling t)) (string= "Projects" (org-get-category)))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

    (defun org-current-is-todo ()
      (string= "TODO" (org-get-todo-state)))

    (defun my-org-has-child-p ()
      (interactive)
      (save-excursion
        (org-goto-first-child)))

    (defun my-org-agenda-skip-all-but-project-heading ()
      (let (should-skip-entry)
        (unless(my-org-has-child-p))
          (setq should-skip-entry t))
      )


      ;; org-agenda changes
      (use-package org-super-agenda
        :ensure t
        :defer 5
        :config
        (add-to-list 'org-agenda-custom-commands
                     '("n" "Next View"
                       ((agenda "" ((org-agenda-span 'day)
                                    (org-super-agenda-groups
                                     '((:name "Today"
                                              :time-grid t
                                              :todo "TODAY"
                                              :scheduled today
                                              :order 0)
                                       (:habit t)
                                       (:name "Due Today"
                                              :deadline today
                                              :order 2)
                                       (:name "Due Soon"
                                              :deadline future
                                              :order 8)
                                       (:name "Overdue"
                                              :deadline past
                                              :order 7)
                                       ))))
                        (todo "" ((org-agenda-overriding-header "")
                                  (org-super-agenda-groups
                                   '((:discard (:not (:tag ("urgent"))))
                                     (:name "Urgent"
                                            :file-path "/OrgNotes/gtd"
                                            :tag ("urgent")
                                            :order 0
                                            )
                                     (:auto-category t
                                                     :order 9)
                                     ))))

                        (todo "" ((org-agenda-overriding-header "")
                                  (org-super-agenda-groups
                                   '((:discard (:not (:tag "important")))
                                     (:name "Important"
                                            :file-path "/OrgNotes/gtd"
                                            :tag ("important")
                                            :order 0
                                            )
                                     (:auto-category t
                                                     :order 9)
                                     ))))

                        (todo "" ((org-agenda-overriding-header "")
                                  (org-super-agenda-groups
                                   '((:discard (:not (:todo ("WAITING" "NEXT"))))
                                     (:name "Finish these Task"
                                            :file-path "/OrgNotes/gtd"
                                            :tag ("important")
                                            :order 0
                                            )
                                     (:auto-category t
                                                     :order 9)
                                     ))))

                        (todo "" ((org-agenda-overriding-header "")
                                  (org-super-agenda-groups
                                   '( (:discard (:not (:priority ("A" "B"))))
                                      (:name "Priority"
                                             :file-path "/OrgNotes/gtd"
                                             :priority ("A" "B")
                                             :order 0
                                             )
                                      (:auto-category t
                                                      :order 9)
                                      )))))))
        (org-super-agenda-mode)
        (org-agenda nil "a"))

;;(setq org-agenda-prefix-format
;;      '((agenda . " %i %-12:c%?-12t% s")
;;        (todo   . " %i %-12:c")
    ;;        (tags   . " %i %-12:c")
;;        (search . " %i %-12:c")))

;; picked from [[https://zzamboni.org/post/beautifying-org-mode-in-emacs/]]
;; org-mode to hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
    (setq org-hide-emphasis-markers t)

    ;;The org-bullets package replaces all headline markers with different Unicode bullets:
    (use-package org-bullets
      :ensure t
      :config
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

    ;;https://emacs.stackexchange.com/questions/44081/how-to-tweak-org-emphasis-alist-to-put-e-g-neon-yellow-over-bold-or-italic
    (setq org-emphasis-alist
      '(("*" (bold :foreground "Orange" ))
        ("/" italic)
        ("_" underline)
        ("=" (:background "maroon" :foreground "white"))
        ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
        ("+" (:strike-through t :foreground "black" :background "white"))))

    ;;https://emacs.stackexchange.com/questions/5889/how-to-highlight-text-permanently-in-org-mode/5892#5892
;; (add-to-list 'org-emphasis-alist
;;              '("*" (:foreground "red")))
;; ;; Underline
;; (add-to-list 'org-emphasis-alist
;;              '("_" (:foreground "yellow")))
;; ;; Italics
;; (add-to-list 'org-emphasis-alist
;;              '("/" (:foreground "green")))
;; ;; Strikethrough
;; (add-to-list 'org-emphasis-alist
             ;; '("+" '(:strike-through t :foreground "black" :background "white")))

;;             '("_" (:foreground "black")))
            ;; '("/" (:foreground "blue")))
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Agenda group tags
(setq org-tag-alist '((:startgrouptag)
                      ("GTD")
                      (:grouptags)
                      ("Control")
                      ("Persp")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("Control")
                      (:grouptags)
                      ("Context")
                      ("Task")
                      (:endgrouptag)))

(setq org-tag-alist-for-agenda '((:startgrouptag)
                                 ("GTD")
                                 (:grouptags)
                                 ("Control") ("Persp")
                                 (:endgrouptag)
                                 (:startgrouptag)
                                 ("Control")
                                 (:grouptags)
                                 ("Context") ("Task")
                                 (:endgrouptag)
                                 (:startgouptag)
                                 ("Context")
                                 (:grouptags)
                                 ("@home") ("@office") ("@call") ("@agenda")
                                 (:endgrouptag)
                                 (:startgrouptag)
                                 ("@office")
                                 (:grouptags)
                                 ("new-project") ("work-reading") ("quick") ("focus") ("follow-up") ("night") ("tech_understanding")
                                 (:endgrouptag)
                                 (:startgrouptag)
                                 ("@home")
                                 (:grouptags)
                                 ("reading") ("personal-project") ("deepthi") ("vihaan") ("maya") ("naveen") ("parents")
                                 (:endgrouptag)
                                 ))

;; fold all but the current headline, Sourcee
(defun org-show-current-heading-tidily ()
  (interactive)  ;Inteactive
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))


;; use prefix M-p for personal function key-bindings http://emacslife.com/read-lisp-tweak-emacs/beginner-3-make-things-more-convenient.html
;;(global-set-key "\M-=" 'org-show-current-heading-tidily)
(global-set-key (kbd "M-p") nil) ;; Remove the old keybinding
(global-set-key (kbd "M-p f") 'org-show-current-heading-tidily)
;;(global-set-key (kbd "M-t w") 'org-show-previous-heading-tidily)
;;(global-set-key (kbd "M-t t") 'transpose-words)


(defun org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))

;;(setq org-speed-commands-user
;;      '(("j" . ded/org-show-next-heading-tidily)
;;        ("l" . ded/org-show-previous-heading-tidily))))

;; JIRA Integration
(use-package org-jira
  :ensure t
  :config
  (unless (file-exists-p "~/local_drive/work/.org-jira")
  (make-directory "~/local_drive/work/.org-jira"))
  (setq jiralib-url "https://meraki.atlassian.net/")
  ;; Configure your authentication:
  ;; For basic auth:
  (setq my-api-token "atlassian_very_secret_token")
  (setq my-api-token (getenv "ATLASSIAN_API_TOKEN"))
  )


;;Project Management
(use-package org-ql
  :ensure t)
(use-package ts
  :ensure t)
(use-package s
  :ensure t)
(use-package dash
  :ensure t)

;; Beautify org-mode
(setq org-startup-indented t
      ;;org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(1100))

(setq org-image-actual-width (/ (display-pixel-width) 2))
;; Change the bullets
(use-package org-superstar
  :ensure t
  :config
;;  ((save-excursion )org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))
;; Increase line spacing for better readability
(setq-default line-spacing 5)

;;
(setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "magenta" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                )))

;; Enabling org-babel support for more languages
 (org-babel-do-load-languages
        'org-babel-load-languages
        '((shell      . t)
          (js         . t)
          (emacs-lisp . t)
          (perl       . t)
          (clojure    . t)
          (python     . t)
          (ruby       . t)
          (dot        . t)
          (css        . t)
          ))


(defun org-to-docx ()
  "Convert current Org buffer to DOCX using Pandoc."
  (interactive)
  (let ((org-file (buffer-file-name))
        (docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx")))
    (shell-command (format "pandoc -s %s -o %s" org-file docx-file))
    (message "Exported to %s" docx-file)))
