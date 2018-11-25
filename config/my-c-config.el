;;; package --- my-c-config.el
;;;
;;; Commentary:
;;;   Provide the C/C++/OC configurations for init.el.
;;;
;;; Code:


(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)

(use-package cc-mode
  :ensure t
  :defer t
  :config
  (define-key c-mode-base-map (kbd "C-d") nil)
  )


(use-package google-c-style
  :ensure t
  :defer t
  :hook
  (c-mode-common . google-set-c-style)
  )


(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt|.cmake"
  )


(use-package lsp-mode
  :ensure t
  :defer t
  :diminish lsp-mode "Ⓛ"
  )


(use-package cquery
  :ensure t
  :commands lsp-cquery-enable
  :init
  (defun cquery-enable () (condition-case nil (lsp-cquery-enable) (user-error nil)))
  :hook
  (c-mode   . (lambda() (cquery-enable)))
  (c++-mode . (lambda() (cquery-enable)))
  :config
  (defvar emacs-config-dir "~/.emacs.config/cquery/build/release/bin/")
  (setq cquery-executable (concat emacs-config-dir (if is-windows? "cquery.exe" "cquery")))
  (setq cquery-cache-dir "~/.emacs.config/.cquery.cache")
  (setq cquery-extra-args '("--log-file=~/AppData/Roaming/.emacs.config/.cquery.log/cquery.log"))
  (setq cquery-extra-init-params '(:extraClangArguments ("-driver-mode=cl")))
  )


(use-package company-lsp
  :ensure t
  :defer t
  :init
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-enable-recompletion t)
  (setq company-lsp-async nil)
  (setq company-transformers nil)
  :config
  (push 'company-lsp company-backends)
  )


(use-package omnisharp
  :ensure t
  :defer t
  :diminish omnisharp-mode "Ⓞ"
  :config
  (when is-windows? (setq omnisharp-server-executable-path "~/.emacs.config/omnisharp/OmniSharp.exe"))
  (omnisharp-mode)
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (eval-after-load 'company '(add-to-list 'company-backends 'company-omnisharp))
  (add-hook 'csharp-mode-hook #'company-mode)
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
  )

(provide 'my-c-config)
;;; my-c-config.el ends here
