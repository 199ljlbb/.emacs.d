;;; package --- prog-packages.el
;;;
;;; Commentary:
;;;   The packages for programming.
;;;
;;; Code:


(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (declare-function projectile-mode "projectile")
  (declare-function projectile-project-root "projectile")
  :config
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  :hook (after-init . projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))


(use-package counsel-projectile
  :ensure t
  :init (declare-function counsel-projectile-mode "counsel-projectile")
  :hook (after-init . counsel-projectile-mode))


(use-package treemacs
  :ensure t
  :defer t
  :bind ("M-k M-b" . treemacs))


(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)


(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))


(use-package smartparens
  :ensure t
  :init
  (declare-function smartparens-global-mode "smartparens")
  (declare-function sp-local-pair "smartparens")
  (show-paren-mode t)
  (setq-default show-paren-delay 0)
  :diminish smartparens-mode
  :config
  (defadvice show-paren-function (around fix-show-paren-function activate)
    (cond ((looking-at-p "\\s(") ad-do-it)
          (t (save-excursion
               (ignore-errors (backward-up-list))
               ad-do-it))))
  (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
  :hook (prog-mode . smartparens-mode))


(use-package nlinum
  :ensure t
  :init
  (declare-function global-nlinum-mode "nlinum")
  (declare-function nlinum-mode "nlinum")
  :config (setq nlinum-format "%4d")
  :hook (prog-mode . nlinum-mode))


(use-package company
  :ensure t
  :diminish company-mode "Ⓒ"
  :init
  (declare-function global-company-mode "company")
  (declare-function company-mode "company")
  :hook
  (after-init . global-company-mode)
  (gdb-mode             . (lambda() (company-mode 0)))
  (eshell-mode          . (lambda() (company-mode 0)))
  (shell-mode           . (lambda() (company-mode 0)))
  (python-mode          . (lambda() (company-mode 0)))
  (inferior-python-mode . (lambda() (company-mode 0)))
  (ein:notebook-mode    . (lambda() (company-mode 0))))


(use-package flycheck
  :ensure t
  :diminish flycheck-mode "Ⓕ"
  :init (declare-function global-flycheck-mode "flycheck")
  :hook (after-init . global-flycheck-mode))


(use-package csv-mode
  :ensure t
  :defer t)


(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(use-package lsp-mode
  :ensure t)


(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))


(provide 'prog-packages)
;;; prog-packages.el ends here
