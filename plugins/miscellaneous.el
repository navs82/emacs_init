;; Copy withour Selections


;; Base Functions
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))


;; Copy Word
(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

;; Key Binding
(global-set-key (kbd "C-c w")         (quote copy-word))

;; Copy Backward Word
(defun copy-backward-word ()
  "copy word before point - rocky @ stackexchange"
  (interactive "")
  (save-excursion
    (let ((end (point))
          (beg (get-point 'backward-word 1)))
      (copy-region-as-kill beg end))))

;; Key binding
(global-set-key (kbd "C-c W") 'copy-backward-word)

;; Copy Line
(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;;(paste-to-mark arg)
  )

;; Key Binding
(global-set-key (kbd "C-c L")         (quote copy-line))

;;Copy Paragraph
 (defun copy-paragraph (&optional arg)
   "Copy paragraphes at point"
   (interactive "P")
   (copy-thing 'backward-paragraph 'forward-paragraph arg)
   (paste-to-mark arg)
   )

(global-set-key (kbd "C-c P")         (quote copy-paragraph))

;;Copy String
(defun beginning-of-string (&optional arg)
(when (re-search-backward "[ \t]" (line-beginning-position) :noerror 1)
  (forward-char 1)))
(defun end-of-string (&optional arg)
(when (re-search-forward "[ \t]" (line-end-position) :noerror arg)
  (backward-char 1)))

(defun thing-copy-string-to-mark(&optional arg)
" Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
(interactive "P")
(copy-thing 'beginning-of-string 'end-of-string arg)
(paste-to-mark arg)
)

(global-set-key (kbd "C-c s")         (quote thing-copy-string-to-mark))

;; Copy Parenthesis
 (defun beginning-of-parenthesis (&optional arg)
      (when (re-search-backward "[[<(?\"]" (line-beginning-position) :noerror)
        (forward-char 1)))

(defun end-of-parenthesis (&optional arg)
  (when (re-search-forward "[]>)?\"]" (line-end-position) :noerror arg)
    (backward-char 1)))

(defun thing-copy-parenthesis-to-mark (&optional arg)
  " Try to copy a parenthesis and paste it to the mark
     When used in shell-mode, it will paste parenthesis on shell prompt by default "
  (interactive "P")
  (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
  (paste-to-mark arg)
  )
(global-set-key (kbd "C-c a")         (quote thing-copy-parenthesis-to-mark))

(defun reload-all-buffers ()
  "Reload all open buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t)))))
(global-set-key (kbd "<f5>") 'reload-all-buffers)


(defun copy-week6-table ()
  "Copy the org-mode table named week6 to the clipboard."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "week6")
    (org-table-copy-region (org-table-begin) (org-table-end))
    (message "Table 'week6' copied to clipboard.")))


(defun org-copy-table (table-name)
  (let ((table (org-table-find table-name)))
    (if table
        (progn
          (setq org-table-clip (org-table-to-lisp table))
          (message "Table '%s' copied to clipboard." table-name))
      (message "Table '%s' not found." table-name))))


(defun my/org-archive-done-tasks ()
  "Archive all headlines marked as DONE in the current Org file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ DONE " nil t)  ;; Find "DONE" headlines
      (org-archive-subtree))))
