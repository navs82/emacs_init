;;;; hook lsp-mode for languages

;;(require 'lsp-clangd)

(use-package lsp-mode
  :config
  :commands lsp
  :init
;;  (lsp-register-client clangd)
  (add-hook 'c++-mode-hook #'lsp)
 ;; (add-hook 'python-mode-hook #'lsp)
 ;; (add-hook 'rust-mode-hook #'lsp)
 ;; (setq lsp-disabled-client '(ccls))
  (setq lsp-clients-clangd-executable "/usr/bin/clangd-9")
   ;; `-background-index' requires clangd v8+!
   (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=verbose" ))
   (setq lsp-log-io t)
   (setq lsp-print-io t)
 ;;  (setq lsp-auto-require-clients nil)
   (setq lsp-auto-guess-root t)
 ;;  (setq lsp-auto-configure nil)
   ;;((c++-mode . ((lsp-enabled-clients . (clangd)))))
)
