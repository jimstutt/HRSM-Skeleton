#!/usr/bin/env bash
set -euo pipefail

DIR="$HOME/Dev/HRSM-Skeleton"
mkdir -p "$DIR"

cat << 'ELISPEOF' > "$DIR/emacs-gptel-hrsm.el"
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
    load-f
  ;; Set as the active default model for gptel
  (setq gptel-model "hrsm-assistant"
        gptel-backend (gptel-get-backend "Ollama-HRSM")))
ELISPEOF

echo "[HRSM] Emacs configuration file generated at $DIR/emacs-gptel-hrsm.el"
