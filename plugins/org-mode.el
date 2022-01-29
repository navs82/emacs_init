
;; setting global key for org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
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

;; todo->not started, Next->should be picked up next, WWAITING: work is started bu not finished, DONE->Finished
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")))

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
                                 (:startgrouptag)
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
  (make-directory "~/local_drive/.org-jira")
  (setq jirlib-url "https://jira.ikare.io"))


;;Project Management
(use-package org-ql
  :ensure t)
(use-package ts
  :ensure t)
(use-package s
  :ensure t)
(use-package dash
  :ensure t)
