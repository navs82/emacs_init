;;; initfile --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested
;;; Code:
;;Save current opened session

(require 'desktop)
(setq desktop-path (list "~/emacs-server"))
(desktop-save-mode 1)
;; Enable mouse in the terminal
(xterm-mouse-mode 1)
(defun track-mouse (e))
(setq mouse-sel-mode t)
;;(setq view-read-only t)
;; toggle view mode
;;(add-hook 'find-file-hook (lambda ()
;;                            (if (eq major-mode 'c++-mode)
;;                            (setq buffer-read-only t)))) ;;(setq buffer-read-only t)))
;;(define-key ctl-x-map "\C-q" 'view-mode)
(define-key ctl-x-map "\C-q" 'read-only-mode)

;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration/Customization:
;; Defines global variables that are later used to customize and set
;; up packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specify the ycmd server command and path to the ycmd directory *inside* the
;; cloned ycmd directory
;;(defvar my:ycmd-server-command '("python3" "/home/nanand/git/ycmd/ycmd"))
;;(defvar my:ycmd-extra-conf-whitelist '("/home/nanand/git/ycmd/my_config/ycm_extra_conf.py"))
;;(defvar my:ycmd-global-config "/home/nanand/git/ycmd/my_config/.ycm_extra_conf.py")

;; Specify the jupyter executable name, and the start dir of the server
;;(defvar my:jupyter_location (executable-find "jupyter"))
;;(defvar my:jupyter_start_dir "/home/nils")

;; Compilation command for C/C++
(defvar my:compile-command "clang++ -Wall -Wextra -std=c++14 ")

;;Configuring GnuPG
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/opt/homebrew/bin/gpg"))
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
(setq org-crypt-key nil)
;; GPG key to use for encryption.
;; nil means  use symmetric encryption unconditionally.
;; "" means use symmetric encryption unless heading sets CRYPTKEY property.

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "http://elpa.gnu.org/packages/")))

;; Disable package initialize after us.  We either initialize it
;; anyway in case of interpreted .emacs, or we don't want slow
;; initizlization in case of byte-compiled .emacs.elc.
(setq package-enable-at-startup nil)
;; Ask package.el to not add (package-initialize) to .emacs.
(setq package--init-file-ensured t)
;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))
;; Add the macro generated list of package.el loadpaths to load-path.
(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (require 'package)
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        ;; (require 'use-package)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Extra plugins and config files are stored here
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/elgantt/"))
(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
;;Load other files that is needed
(load  "miscellaneous")
;; Load lsp
(load "clangd")
;;(load "rtags")
;;Load other files that is needed
(load  "miscellaneous")
(load "org-mode")
(load "python-config")
;;(require 'websocket)
;;(require 'simple-httpd)
;;(load-library "org-roam-ui")
(require 'elgantt)
(setq elgantt-agenda-files (concat user-emacs-directory "private/elgantt/test.org"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(if (and (fboundp 'server-running-p)
;;        (not (server-running-p)))
;;    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)
;; Disable the toolbar at the top since it's useless
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
          (lambda ()
            (when (not (derived-mode-p 'ein:notebook-multilang-mode))
              (delete-trailing-whitespace))))

;; Auto-wrap at 120 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 150)
(turn-on-auto-fill)
;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Global Keyboard Shortcuts
;; Set mark paragraph to M-?
;; case insensitive search
(setq completion-ignore-case t)
;; Use meta+tab word completion
(global-set-key (kbd "M-TAB") 'dabbrev-expand)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; Indent after a newline, if required by syntax of language
(global-set-key (kbd "C-m") 'newline-and-indent)
;; Load the compile ocmmand
;;(global-set-key (kbd "C-c C-c") 'compile)
;; Undo, basically C-x u
(global-set-key (kbd "C-/") 'undo)
;; Find file in project
(global-set-key (kbd "C-x M-f") 'project-find-file)
;; highlight the current symbol
(global-set-key (kbd "M-*") 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)
;; Disable the menu bar since we don't use it, especially not in the
;; terminal
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))
            )
          )

;;
;;USING PGP
(setq epg-gpg-program "gpg2")
;; Setup use-package
(eval-when-compile
  (require 'use-package))
(use-package bind-key
  :ensure t)
;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable terminal emacs to copy and paste from system clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this uses C-c before the usual C-w, M-w, and C-y
;; From: https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux
(defun my-copy-to-xclipboard(arg)
  (interactive "P")
  (cond
   ((not (use-region-p))
    (message "Nothing to yank to X-clipboard"))
   ((and (not (display-graphic-p))
         (/= 0 (shell-command-on-region
                (region-beginning) (region-end) "xsel -i -b")))
    (message "Error: Is program `xsel' installed?"))
   (t
    (when (display-graphic-p)
      (call-interactively 'clipboard-kill-ring-save))
    (message "Yanked region to X-clipboard")
    (when arg
      (kill-region  (region-beginning) (region-end)))
    (deactivate-mark))))

(defun my-cut-to-xclipboard()
  (interactive)
  (my-copy-to-xclipboard t))

(defun my-paste-from-xclipboard()
  (interactive)
  (if (display-graphic-p)
      (clipboard-yank)
    (insert (shell-command-to-string "xsel -o -b"))))

(global-set-key (kbd "C-c C-w") 'my-cut-to-xclipboard)
(global-set-key (kbd "C-c M-w") 'my-copy-to-xclipboard)
(global-set-key (kbd "C-c C-y") 'my-paste-from-xclipboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s is used by ycmd, origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically compile and save ~/.emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-compile-init-files (file)
  "Automatically compile FILE."
  (interactive)
  (save-restriction
    ;; Suppress the warning when you setq an undefined variable.
    (if (>= emacs-major-version 23)
        (setq byte-compile-warnings '(not free-vars obsolete))
      (setq byte-compile-warnings
            '(unresolved
              callargs
              redefine
              obsolete
              noruntime
              cl-warnings
              interactive-only)))
    (byte-compile-file (expand-file-name file)))
  )

(add-hook
 'after-save-hook
 (function
  (lambda ()
    (if (string= (file-truename "~/.emacs.el")
                 (file-truename (buffer-file-name)))
        (byte-compile-init-files (file-truename "~/.emacs.el")))
    )
  )
 )

;; Byte-compile again to ~/.emacs.elc if it is outdated
(if (file-newer-than-file-p
     (file-truename "~/.emacs.el")
     (file-truename "~/.emacs.elc"))
    (byte-compile-init-files "~/.emacs.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :commands (ivy-mode)
  :config
  (require 'ivy)
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap nil)
  (add-hook 'org-mode-hook #'flyspell-mode)
;;            (lambda() (ivy-wrap -1 message "org-mode-hook-called")))
  (add-hook 'c++-mode-hook (lambda()(ivy-wrap t)))
  (add-hook 'c++-mode-hook (lambda () (set-fill-column 120)))
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  (setq projectile-completion-system 'ivy)
  (setq ivy-display-style 'fancy)
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper))
  )

;;       ("C-x C-f" . counsel-find-file)
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git-grep)
         ("C-c j" . counsel-git)
         ("C-c k" . counsel-ag)
         ("C-c f" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-c i" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-add)
         ("C-c i" . counsel-imenu) ;; don't know how to get here, and what is map minibuffer-local-map for
         )
  :config
  (if (executable-find "rg")
      ;; use ripgrep instead of grep because it's way faster
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            counsel-rg-base-command
            "rg -i -M 120 --no-heading --line-number --color never %s . -g '!*.testie' -g '!TAGS' -g '!BROWSE'"
            )
    (warn "\nWARNING: Could not find the ripgrep executable. It "
          "is recommended you install ripgrep.")
    )

  (global-set-key (kbd "<f7>") 'counsel-imenu)
  )

;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :ensure t
  :bind (
       ;;("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :config
  ;; Ignore files above 800kb
  ;;(setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180)
  ;; Set up auto-update
  (add-hook
   'prog-mode-hook
   (lambda () (add-hook 'after-save-hook
                        (lambda ()
                          (counsel-etags-virtual-update-tags))))
   )

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (shell-command cmd)
        (visit-tags-table tags-file t)
        )
      )
    )

  (setq counsel-etags-update-tags-backend
        (lambda ()
          (interactive)
          (let* ((tags-file (counsel-etags-locate-tags-file)))
            (when tags-file
              (my-scan-dir (file-name-directory tags-file) t)
              (run-hook-with-args
               'counsel-etags-after-update-tags-hook tags-file)
              (unless counsel-etags-quiet-when-updating-tags
                (message "%s is updated!" tags-file))))
          )
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package window-numbering installed from package list
;; Allows switching between buffers using meta-(# key)
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode t)
  )

;;(use-package transpose-frame
;;  :defer t
;;  :init
;;  (leader-def-key "wt" 'transpose-frame))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit server to allow editing of things in Chrome with Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package edit-server
  :ensure t
  :defer 10
  :config
  (progn
    (eval-when-compile
      ;; Silence missing function warnings
      (declare-function edit-server-start "edit-server-start.el"))
    (when (daemonp)
      (edit-server-start)
      )
    (add-hook 'edit-server-start-hook
              (lambda ()
                (when (string-match "github.com" (buffer-name))
                  (markdown-mode))))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

;; Reasonable splitting on wide screens
(setq split-height-threshold 30)
(setq split-width-threshold  160)
;; Load rtags and start the cmake-ide-setup process
;; Do not install the following packages using MELPA.
;; These packages should be installed by hand using `make install' from
;; rtags source directory.
;;
;; For more see URL `https://github.com/Andersbakken/rtags/issues/1318'.

(use-package rtags
    :ensure t
    ;; :init
    ;; (add-to-list 'display-buffer-alist
    ;;            '("*RTags*"
    ;;              (display-buffer-below-selected)
    ;;              (inhibit-same-window . t)
    ;;              (window-height . 30)))
    ;; (add-to-list 'display-buffer-alist
    ;;              '("*rdm*"
    ;;                (display-buffer-in-side-window)
    ;;                (side . top)
    ;;                (inhibit-same-window . t)
    ;;                (window-height . 5)))
    :bind (
           ;;("C-x C-f" . rtags-find-file)
           ("C-c r 8" . rtags-find-symbol-current-file)
           ("C-c r 7" . rtags-find-symbol-current-dir)
           ;;("M-." . rtags-find-symbol-at-point)
           ;;("M-," . rtags-find-references-at-point)
           ;;("C-c [" . rtags-location-stack-back)

           )
    :config
    (setq rtags-enable-unsaved-reparsing nil) ;; nil is default value
    (setq rtags-reindex-on-save nil) ;; rtags will not be reparsed when saving file
    (setq rtags-completions-enabled t)
    (setq rtags-show-containing-function t)
    (setq  rtags-completions-timer-interval 0.5)
    ;;(setq rtags-periodic-reparse-timeout 1.5)
    (rtags-enable-standard-keybindings)
    (setq rtags-other-window-function #'(lambda () (other-window -1)))
    (setq rtags-results-buffer-other-window t)
    ;; open files in view mode only
    ;;(add-hook 'find-file-hook (lambda () (setq view-mode t))) ;;(setq buffer-read-only t)))
    (setq rtags-multiple-targets t) ;; for multiple target
    (setq rtags-use-helm t)
    ;;(add-hook 'after-save-hook (lambda() (setq view-mode t))) ;; make is only read buffer
    )
;; set up irony-mode
;;(use-package irony
;;    :ensure t
;;    :init
;;
;;    (add-hook 'c++-mode-hook 'irony-mode)
;;    (add-hook 'c-mode-hook 'irony-mode)
;;    (add-hook 'objc-mode-hook 'irony-mode)
;;    (defun my-irony-mode-hook ()
;;      (define-key irony-mode-map [remap completion-at-point]
;;        'irony-completion-at-point-async)
;;      (define-key irony-mode-map [remap complete-symbol]
;;        'irony-completion-at-point-async))

    ;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
    ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup cmake-ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package cmake-ide
;;    :ensure t
;;    :disabled
;;   :init
;;    (eval-and-compile
      ;; Silence missing function warnings
;;      (declare-function cmake-ide-mode "cmake-ide.el")))
;;(require 'cmake-ide)
;;(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
;;(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; We want to be able to compile with a keyboard shortcut
;;(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html
;; Naveen start use company-rtags
;;(setq rtags-autostart-diagnostics t)
;;(rtags-diagnostics)
;;(setq rtags-completions-enabled t)
;;(rtags-enable-standard-keybindings)
;;naveen end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :defer 10
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :ensure t
  :config
  (beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :defer 10
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char)
         ("M-s" . avy-goto-word-1))
  ;; Set keys for Dvorak mode instead of qwerty
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s
                            ?A ?O ?E ?U ?I ?D ?H ?T ?N ?S
                            ?p ?y ?f ?g ?c ?r ?l
                            ?P ?Y ?F ?G ?C ?R ?L)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zzz-to-char: replaces the built-in zap-to-char with avy-like
;;              replacement options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zzz-to-char
  :ensure t
  :bind ("M-z" . zzz-up-to-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :disabled
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))
(setq-default pdb-command-name "python -m pdb")
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :after python
  :config
  (elpy-enable)
  )

(use-package yapfify
  :ensure t
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern C++ code highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package modern-cpp-font-lock
  :ensure t
  :defer 10
  :config
  (modern-c++-font-lock-global-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :defer 10
  :config
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)
  (use-package google-c-style
    :ensure t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    (add-hook 'c++-mode-hook 'google-make-newline-indent)
    (add-hook 'c++-mode-hook 'google-set-c-style)

    )
  )

;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)

;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 4)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 4)

;; We want to be able to see if there is a tab character vs a space.
;; global-whitespace-mode allows us to do just that.
;; Set whitespace mode to only show tabs, not newlines/spaces.
(use-package whitespace
  :ensure t
  :init
  (eval-when-compile
      ;; Silence missing function warnings
      (declare-function global-whitespace-mode "whitespace.el"))
  :config
  (setq whitespace-style '(tabs tab-mark))
  ;; Turn on whitespace mode globally.
  (global-whitespace-mode t)
  )

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: ycmd (YouCompleteMeDaemon)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up YouCompleteMe for emacs:
;; https://github.com/Valloric/ycmd
;; https://github.com/abingham/emacs-ycmd
;;(defvar my:python-location (executable-find (nth 0 my:ycmd-server-command)))
;;(if (not my:python-location)
;;    (message
;;     "Could not start YouCompleteMeDaemon because the python executable could
;;not be found.\nSpecified executable is: '%s'\nPlease set my:ycmd-server-command
;;appropriately in ~/.emacs.el.\n" (nth 0 my:ycmd-server-command)))
;;(if (not (file-directory-p (nth 1 my:ycmd-server-command)))
;;    (message "Could not YouCompleteMeDaemon because the specified directory does
;;not exist.\nSpecified directory is: '%s'
;;Please set my:ycmd-server-command appropriately in ~/.emacs.el.\n"
;;            (nth 1 my:ycmd-server-command)))
;;(if (and my:python-location
;;         (file-directory-p (nth 1 my:ycmd-server-command)))
;;    (use-package ycmd
;;      :ensure t
;;      :init
;;      (eval-when-compile
        ;; Silence missing function warnings
;;        (declare-function global-ycmd-mode "ycmd.el"))
;;      (add-hook 'after-init-hook #'global-ycmd-mode)
;;     :config
;;      (progn
;;        (set-variable 'ycmd-server-command my:ycmd-server-command)
;;        ;;(set-variable 'ycmd-extra-conf-whitelist my:ycmd-extra-conf-whitelist)
;;        (set-variable 'ycmd-global-config my:ycmd-global-config)
;;        (setq ycmd-force-semantic-completion t)
;;       (use-package company-ycmd
;;          :ensure t
;;          :init
;;          (eval-when-compile
            ;; Silence missing function warnings
;;            (declare-function company-ycmd-setup "company-ycmd.el"))
;;          :config
;;          (company-ycmd-setup)
;;          )

;;        (use-package flycheck-ycmd
;;          :ensure t
;;          :init
;;          (add-hook 'c-mode-common-hook 'flycheck-ycmd-setup)
;;          )

        ;; Add displaying the function arguments in mini buffer using El Doc
;;        (require 'ycmd-eldoc)
;;        (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
;;        )
;;      )
;;  )

;; Hybrid Setup: Corfu + Company (Best of both worlds)
;; You can use Corfu for lightweight completion and Company for LSP-heavy coding:

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up code completion with company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  :hook (prog-mode . company-mode)
  ;; enable completion preview mode release in emacs 30.1
  (completion-preview-mode 1)
  ;; Zero delay when pressing tab
  (setq company-idle-delay 0.10)
  (setq company-async-timeout 10)
  ;;(define-key c-mode-map [(tab)] 'company-complete)
  ;;(define-key c++-mode-map [(tab)] 'company-complete)
  ;; (define-key c-mode-map [(backtab)] 'company-complete)
  (define-key c++-mode-map [(backtab)] 'company-complete)

  (add-hook 'after-init-hook 'global-company-mode)
  ;; remove unused backends
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )
;;Use rtags for auto completion
(use-package company-rtags
;;  :require company rtags
  :ensure t
  :after rtags
  :config
  ;(setq rtags-autostart-diagnostics t)
  ;(rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (global-company-mode)
  ;;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  ;;(define-key c++-mode-map (kbd "<C-tab>") (function company-complete))
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
)

;; Ivy to view the result of rtags
(use-package ivy-rtags
  :ensure t
  :after rtags
  :config
  (setq rtags-display-result-backend 'ivy)
)

;; use irony-mode for auto-completion
(use-package company-irony
  :ensure t
  :config
  ;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  ;;(eval-after-load 'company
  ;;  '(add-to-list
  ;;    'company-backends 'company-irony))
)

(use-package click-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.template\\'" . click-mode))
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . click-mode))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck-ycmd
(use-package flycheck
  :ensure t
  :defer t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-flycheck-mode "flycheck.el"))
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  (when (not (display-graphic-p))
    (setq flycheck-indication-mode nil))
  )
(use-package flycheck-pyflakes
  :ensure t
  :after python)
;;
;;eldoc-mode
;;
(use-package irony-eldoc
  :ensure t
  :after irony
  (add-hook 'irony-mode-hook 'irony-eldoc)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-inflection
;; used for switching between different cases, eg CamelCase,
;; lowerCamelCase, snake_case, and SCREAMING_SNAKE_CASE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package string-inflection
  :ensure t
  :defer t
  :bind (:map c++-mode-map
         ("C-c c i" . string-inflection-cycle)
         ("C-c c l" . string-inflection-lower-camelcase)
         ("C-c c c" . string-inflection-camelcase)
         ("C-c c s" . string-inflection-underscore)
         ("C-c c u" . string-inflection-upcase)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (:map c++-mode-map
         ("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c a" . mc/mark-all-like-this)
         ("C-c e" . mc/edit-lines))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(use-package writegood-mode
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function writegood-mode "writegood-mode.el"))
  (add-hook 'org-mode-hook #'writegood-mode)
  ;;ref https://stackoverflow.com/questions/36359943/how-i-can-change-column-view-org-mode-agenda-default-settings
  (setq org-agenda-overriding-columns-format "%TODO %PRIORITY  %150ITEM 100%TAGS")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Roam V2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam
      :ensure t
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory (expand-file-name  "~/local_drive/personal/OrgNotes"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today)
             ("C-c n y" . org-roam-dailies-goto-yesterday)
             ("C-c n t" . org-roam-dailies-goto-today)
             ("C-c n d" . org-roam-db-sync))
      :config
      (org-roam-db-autosync-mode)
      (setq org-roam-db-location  (expand-file-name "~/local_drive/references/OrgNotes/org-roam.db"))
      ;;BacklinksView (preview of) nodes that link to this node
      (setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            #'org-roam-unlinked-references-section
            ))
      (setq org-roam-dailies-directory "daily/")
      ;;new daily template
      ;;(setq org-roam-dailies-capture-templates
      ;;      '(("d" "daily" entry #'org-roam-capture--get-point
      ;;         "* %?\n")))
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-edna
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( use-package org-edna
  :ensure t
  :init )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load hungry Delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set hungry delete:
(use-package hungry-delete
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-hungry-delete-mode "hungry-delete.el"))
  :config
  (global-hungry-delete-mode t)
  )

;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function flyspell-goto-next-error "flyspell.el")
    (declare-function flyspell-mode "flyspell.el")
    (declare-function flyspell-prog-mode "flyspell.el"))
  (setq flyspell-issue-welcome-flag nil)
  :config
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

;;  (global-set-key (kbd "<f7>") 'flyspell-buffer)
  (global-set-key (kbd "<f8>") 'flyspell-correct-previous)
  (global-set-key (kbd "<f9>") 'flyspell-correct-previous)

  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
  )

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :requires dash
  :after (ivy)
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :config
  (add-hook 'magit-mode-hook (lambda () (setq whitespace-mode -1)))
  (setq magit-completing-read-function 'ivy-completing-read)
  )
(use-package magit-gerrit
  :ensure t
  :after magit
  )

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-enable-caching t)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GitGutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function global-git-gutter-mode "git-gutter.el"))
  :config
  ;; If you enable global minor mode
  (global-git-gutter-mode t)
  ;; Auto update every 5 seconds
  (custom-set-variables
   '(git-gutter:update-interval 5))

  ;; Set the foreground color of modified lines to something obvious
  (set-face-foreground 'git-gutter:modified "purple")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  :config
  (use-package cmake-font-lock
    :ensure t
    :defer t
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          (font-lock-add-keywords
                           nil '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                                  1 font-lock-warning-face t)))
                          ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode (".yml" ".yaml"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode (".json" ".imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function yas-global-mode "yasnippet.el"))
  :defer 5
  :config
  (yas-global-mode t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all)
  ;;(setq yas-snippet-dirs '( "~/local_drive/personal/snippets/org-mode"))
;;  (setq yas-snippet-dirs (append
;;                         (expand-file-name "~/local_drive/personal/snippets/org-mode")))
  )
;; Apparently the company-yasnippet backend shadows all backends that
;; come after it. To work around this we assign yasnippet to a different
;; keybind since actual source completion is vital.
(use-package company-yasnippet
  :bind ("C-M-y" . company-yasnippet)
  :after (yasnippet)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :mode (".md" ".markdown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; When we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-source-correlate-start-server t)
  (cond
   ((string-equal system-type "windows-nt") ; Microsoft Windows
    (progn
      (message "Windows does not have a PDF viewer set for auctex")))
   ((string-equal system-type "darwin") ; Mac OS X
    (setq-default
     TeX-view-program-list
     '(("Skim"
        "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
       )
     TeX-view-program-selection '((output-pdf "Skim"))))
   ((string-equal system-type "gnu/linux") ; linux
    (setq-default TeX-view-program-list
                  '(("Evince" "evince --page-index=%(outpage) %o"))
                  TeX-view-program-selection '((output-pdf "Evince")))))
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq-default reftex-plug-into-AUCTeX t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The deeper blue theme is loaded but the resulting text
;; appears black in Aquamacs. This can be fixed by setting
;; the font color under Menu Bar->Options->Appearance->Font For...
;; and then setting "Adopt Face and Frame Parameter as Frame Default"
(use-package sourcerer-theme
  :ensure t
  :config
  (load-theme 'wombat t))


;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

;; Hide the scroll bar
;;(scroll-bar-mode -1)
(defvar my-font-size 140)
;; Make mode bar small
(set-face-attribute 'mode-line nil  :height my-font-size)
;; Set the header bar font
(set-face-attribute 'header-line nil  :height my-font-size)
;;Change the selected buffer color(set-face-attribute 'region nil :background "#666")
(set-face-attribute 'region nil :background "#8b0000" :foreground "#ffffff")
;; Set default window size and position
(setq default-frame-alist
      '((top . 0) (left . 0) ;; position
        (width . 110) (height . 90) ;; size
        ))
;; Enable line numbers on the LHS
;;(global-linum-mode -1)
(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))
;; Set the font to size 9 (90/10).
(set-face-attribute 'default nil :height my-font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which function mode and set the header line to display both the
;; path and the function we're in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(which-function-mode t)

;; Remove function from mode bar
(setq mode-line-misc-info
      (delete (assoc 'which-func-mode
                     mode-line-misc-info) mode-line-misc-info))


(defmacro with-face
    (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; In the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "Create the header string and display it."
  ;; The dark blue in the header for which-func is terrible to read.
  ;; However, in the terminal it's quite nice
  (if (display-graphic-p)
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; Set the header line
  (setq header-line-format

        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]")))
              )
        )
  )
;; Call the header line update
(add-hook 'buffer-list-update-hook
          'sl/display-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline theme where the modes are on the right side.
(use-package powerline
  :ensure t
  :config
  (defun powerline-right-theme ()
    "Setup a mode-line with major and minor modes on the right side."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" face0 'l)
                                       (powerline-buffer-size face0 'l)
                                       (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left face0 face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (center (list (powerline-raw global-mode-string face1 'r)
                                          (powerline-raw "%4l" face1 'r)
                                          (powerline-raw ":" face1)
                                          (powerline-raw "%3c" face1 'r)
                                          (funcall separator-right face1 face0)
                                          (powerline-raw " ")
                                          (powerline-raw "%6p" face0 'r)
                                          (powerline-hud face2 face1)
                                          ))
                            (rhs (list (powerline-raw " " face1)
                                       (funcall separator-left face1 face2)
                                       (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                         (powerline-raw erc-modified-channels-object face2 'l))
                                       (powerline-major-mode face2 'l)
                                       (powerline-process face2)
                                       (powerline-raw " :" face2)
                                       (powerline-minor-modes face2 'l)
                                       (powerline-raw " " face2)
                                       (funcall separator-right face2 face1)
                                       ))
                            )
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (powerline-right-theme)
  )
;; Tramp Setting
(setq tramp-default-method "ssh")

;; Set column width
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-width 2)
  (setq fci-rule-color "darkblue")
  (add-hook 'c-mode-hook 'fci-mode)
  (add-hook 'c++-mode-hook 'fci-mode)
  (add-hook 'objc-mode-hook 'fci-mode)
)


;;;;;;windows management
(use-package window-purpose
  :ensure t
  :config
  ;;(purpose-mode)
  (add-to-list 'purpose-user-mode-purposes
               '((c++-mode . cc)))
;;                (rtags-references-tree-mode . ref)))
  ;;(add-to-list 'purpose-user-name-purposes '(Rtags . cc-rtag))
  ;;(add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
  (setq purpose-use-default-configuration t) ; not really necessary, default is t
  (purpose-compile-user-configuration) ; activates your changes
  ;;(setq pop-up-frames t) ; allows emacs to popup new frames
;; give help buffers the 'popup-frame purpose
  ;;(add-to-list 'purpose-user-mode-purposes
  ;;             '(help-mode . popup-frame))
  ;;(purpose-compile-user-configuration)
;; new rules for buffers with the 'popup-frame purpose
  ;;(add-to-list 'purpose-special-action-sequences
  ;;            '(popup-frame
  ;;             purpose-display-reuse-window-buffer
  ;;             purpose-display-reuse-window-purpose
  ;;             purpose-display-pop-up-frame))
  )

;;;========================================
;;; (E)Lisp development
;;;========================================

(use-package buttercup
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

;; Better shell

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-$" . vterm))

;;;========================================
;;; Common Lisp
;;;========================================

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package paredit
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook) . paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (lisp-mode-hook . rainbow-delimiters-mode))

;; PATH variable is not svailable when started from spotlist, ref:https://www.emacswiki.org/emacs/ExecPath
;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

;; This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)


;; (let ((path (shell-command-to-string ". ~/.zshrc;echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;;; Install epdfinfo via 'brew install pdf-tools --HEAD' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))


(use-package graphviz-dot-mode
  :ensure t
  :defer 10
  :config
  (setq graphviz-dot-indent-width 4))


;;; rfc reader
(use-package rfc-mode
  :ensure t
  :defer 10
  :config
  (setq rfc-mode-directory (expand-file-name "~/local_drive/office_work/rfc/")))

;; hunspell dictinary; Took from https://www.reddit.com/r/emacs/comments/dgj0ae/tutorial_spellchecking_with_hunspell_170_for/
(setq ispell-program-name "hunspell")
;;(setq ispell-hunspell-dict-paths-alist
;;'(("en_US" (expand-file-name "~/local_drive/references/d/dictionary/"))))
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))


(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))
;;Set custom file to store custom variable

(setq custom-file (concat user-emacs-directory "custom.el"))
;;(load custom-file)

(use-package tree-sitter-langs
  :ensure t
  :defer 10)

;; tree-sitter
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package keytar
  :ensure t)

;; (use-package lsp-grammarly
;;   :ensure t
;;   ;;:hook (text-mode . (lambda ()
;;   ;;                     (require 'lsp-grammarly)
;;   ;;                     (lsp)))
;;   :config
;;   ;;(add-hook 'org-mode-hook (lambda() (lsp)))
;;   )  ; or lsp-deferred


;; Default Tramp method
;;(use-package tramp
;;  : ensure t
;;  :config
;; (setq tramp-default-method "ssh"))

;;https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Ledger mode
(use-package ledger-mode
  :ensure t
  :config
  (setq ledger-clear-whole-transaction 1)
  :mode ("\\.dat\\'"
        "\\.ledger\\'")
  :bind (:map ledger-mode-map
              ("C-x C-s" . ledger-save))
  :preface
  (defun ledger-save ()
    "Automatically clean the ledger buffer at each save."
    (interactive)
    (save-excursion
      (when (buffer-modified-p)
        (with-demoted-errors (ledger-mode-clean-buffer))
        (save-buffer))))
  )

(use-package flycheck-ledger
  :after ledger-mode)

(use-package deft
  :ensure t
  :config
  ;;(add-to-list 'load-path "/path/to/deft/repository")
  ;;(setq deft-use-filename-as-title nil)
  (global-set-key (kbd "C-M-S-s-d") 'deft)
  (global-set-key (kbd "C-x C-g") 'deft-find-file)
  ;;:custom
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-directory "~/local_drive/personal/OrgNotes")
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
;;  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  :bind
  ("C-c n d" . deft)
  )

;; Device tree-mode
(use-package dts-mode
  :ensure t
  )
;; Measure startup time:
;; Reference :
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
;;Ediff configuration
(setq ediff-split-window-function 'split-window-horizontally)
;;Scala Support
;; Scala Support
(unless (package-installed-p 'scala-mode)
  (package-install 'scala-mode))

;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;; Setting  indentation for org-mode
;; t: Adapt indentation for all lines
;; headline-data: Adapt indentation for headline data lines
;; nil: Do not adapt indentation at all
(setq org-adapt-indentation t)
;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el in favor of straight.el
;;(setq package-enable-at-startup nil)
;;(straight-use-package 'org-roam)
(straight-use-package 'applescript-mode)
(provide '.emacs)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 ;;'(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "ebd9bea137cafba0138f5a6996aa6851c4ee8263844c75a57798faacbcf8e3e4" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" default))
 '(git-gutter:update-interval 5)
 '(highlight-symbol-foreground-color "black")
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(line-number-mode nil)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(rfc-mode graphviz-dot-mode pdf-tools paredit sly vterm elisp-lint package-lint buttercup lsp-pyright hide-mode-line dap-mode treemacs org-roam rtags eglot click-mode cmake-font-lock lsp-java lsp-clangd hierarchy call-graph transpose-frame window-purpose lsp-ui company-lsp lsp-mode org-mime solarized-theme doom-themes monokai-theme zenburn-theme 0blayout powerline zzz-to-char yasnippet-snippets yapfify yaml-mode writegood-mode window-numbering which-key wgrep web-mode vlf use-package string-inflection sourcerer-theme realgud rainbow-delimiters prognth origami multiple-cursors modern-cpp-font-lock markdown-mode magit-gerrit json-mode hungry-delete google-c-style git-gutter flyspell-correct-ivy flycheck-pyflakes elpy edit-server cuda-mode counsel-etags company-jedi cmake-mode clang-format beacon autopair auto-package-update auctex))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(purpose-mode nil)
 '(safe-local-variable-values '((related-file-name . "../../lib/packet.cc")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(which-function-mode t)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type tty) (background dark)) (:background "nil"))))
 '(company-preview ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-preview-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-scrollbar-bg ((t (:background "#073642" :foreground "#2aa198"))))
 '(company-scrollbar-fg ((t (:foreground "#002b36" :background "#839496"))))
 '(company-template-field ((t (:background "#7B6000" :foreground "#073642"))))
 '(company-tooltip ((t (:background "black" :foreground "DeepSkyBlue1"))))
 '(company-tooltip-annotation ((t (:foreground "#93a1a1" :background "#073642"))))
 '(company-tooltip-common ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-common-selection ((t (:foreground "#93a1a1" :underline t))))
 '(company-tooltip-mouse ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(company-tooltip-selection ((t (:background "DodgerBlue4" :foreground "CadetBlue1"))))
 '(header-line ((t (:background "#003366"))))
 '(ivy-minibuffer-match-face-1 ((((class color) (background light)) (:background "#555555")) (((class color) (background dark)) (:background "#555555"))))
 '(ivy-minibuffer-match-face-2 ((t (:background "#314f30" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#48225b" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#680a0a" :weight bold))))
 '(which-func ((t (:foreground "#8fb28f")))))
