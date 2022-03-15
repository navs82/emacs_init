;; Provides workspaces with file browsing (tree file viewer)
;; and project management when coupled with `projectile`.

;; to solve https://github.com/syl20bnr/spacemacs/issues/8797
(setq python-shell-completion-native-enable nil)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-no-png-images t
        treemacs-width 24)
  :bind ("C-c t" . treemacs))

;; Provide LSP-mode for python, it requires a language server.
;; I use `lsp-pyright`. Know that you have to `M-x lsp-restart-workspace`
;; if you change the virtual environment in an open python buffer.

(use-package lsp-mode
:ensure t
:defer t
:commands (lsp lsp-deferred)
:init (setq lsp-keymap-prefix "C-c l")
:hook (python-mode . lsp-deferred))

;; Provides completion, with the proper backend
;; it will provide Python completion.

(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; Provides visual help in the buffer
;; For example definitions on hover.
;; The `imenu` lets me browse definitions quickly.
(use-package lsp-ui
  :ensure t
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
	    lsp-ui-doc-delay 2)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
	      ("C-c i" . lsp-ui-imenu)))

;; Integration with the debug server
(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))

;; Hide the modeline for inferior python processes
(use-package inferior-python-mode
  :ensure nil
  :hook (inferior-python-mode . hide-mode-line-mode))

;; Required to hide the modeline
(use-package hide-mode-line
  :ensure t
  :defer t)

;; Required to easily switch virtual envs
;; via the menu bar or with `pyvenv-workon`
;; Setting the `WORKON_HOME` environment variable points
;; at where the envs are located. I use miniconda.
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/anaconda3/envs/"))
  ;;(setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

;; Language server for Python
;; Pyright is microsoft langauge server
;; Read the docs for the different variables set in the config.
(use-package lsp-pyright
  :ensure t
  :defer t
  :config
  (setq lsp-clients-python-library-directories '("/usr/" "~/anaconda3/pkgs"))
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t
	lsp-pyright-venv-path "~/anaconda3/envs")
  :hook ((python-mode . (lambda ()
                          (require 'lsp-pyright) (lsp-deferred)))))

;; Format the python buffer following YAPF rules
;; There's also blacken if you like it better.
(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))

;;https://stackoverflow.com/questions/55175916/emacs-and-conda-workaround
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3")))
