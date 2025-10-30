 ;; Copilot core
(use-package copilot
  :ensure t
  ;; turn it on where you actually code
  :hook (prog-mode . copilot-mode)
  :config
  ;; sensible accept keys (TAB only when a suggestion is visible)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB")   'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-]")   'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "M-]")   'copilot-accept-completion-by-line)

  ;; quick controls
  (define-key copilot-completion-map (kbd "M-n")   'copilot-next-completion)
  (define-key copilot-completion-map (kbd "M-p")   'copilot-previous-completion)

  ;; optional: toggle per buffer
  (global-set-key (kbd "C-c C-o") #'copilot-mode))

;; (Optional) Chat UI in Emacs
(use-package copilot-chat
  :ensure t
  :commands (copilot-chat-display)
  :bind (("C-c C-o C-c" . copilot-chat-display)))

;; Configure the backend + default model (OpenAI example)
(use-package gptel
  :ensure t
  :config
  ;; Backend: OpenAI (change to OpenRouter/local if you prefer)
  (setq gptel-backend
        (gptel-make-openai "openai"
          :key (or (getenv "OPENAI_API_KEY")
                   (cadr (auth-source-user-and-password "api.openai.com")))
          :host "api.openai.com"
          :endpoint "/v1/chat/completions"
          ;; Add supported models here
          :models '(gpt-5-mini, gpt-5-nano, gpt-4o-mini gpt-4o gpt-4.1 gpt-4.1-mini gpt-3.5-turbo)))

  ;; IMPORTANT: model is buffer-local; set a global default STRING
  (setq-default gptel-model 'gpt-4o-mini)

  ;; Stream tokens live (optional but nice)
  (setq gptel-stream t)

  ;; Convenience keys
  (global-set-key (kbd "C-c C-g c") #'gptel)        ;; open chat buffer
  (global-set-key (kbd "C-c C-g r") #'gptel-send)   ;; send region/buffer
  )

(defun my/gptel-ask-about-error ()
  (interactive)
  (let ((msg (or (thing-at-point 'line t) "Explain this.")))
    (gptel-prompt msg)))
(global-set-key (kbd "C-c C-g e") #'my/gptel-ask-about-error)
