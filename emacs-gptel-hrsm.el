;; HRSM-Assistant gptel configuration
;; Load this in your Emacs init.el via: (load-file "~/Dev/HRSM-Skeleton/emacs-gptel-hrsm.el")

(use-package gptel
  :ensure nil ;; Already built via configuration.nix
  :config
  ;; Register the custom Ollama model
  (gptel-make-ollama "Ollama-HRSM"
    :host "localhost:11434"
    :models '("hrsm-assistant")
    :stream t)
    
  ;; Set as the active default model for gptel
  (setq gptel-model "hrsm-assistant"
        gptel-backend (gptel-get-backend "Ollama-HRSM")))
