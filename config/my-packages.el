;;; package --- my-packages.el
;;;
;;; Commentary:
;;;   Provide the necessary packages for init.el.
;;;
;;; Code:


(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  (setq-default ivy-use-virtual-buffers t)

  (use-package swiper
    :ensure t
    :config
    (defun sandric/swiper-or-region (beg end)
      "Swiper region or 'empty string' if none highlighted."
      (interactive (if (use-region-p)
                       (list (region-beginning) (region-end))
                     (list nil nil)))
      (if (and beg end)
          (progn
            (deactivate-mark)
            (swiper (buffer-substring-no-properties beg end)))
        (swiper)))
    :bind
    ("C-s" . sandric/swiper-or-region)
    )

  (use-package counsel
    :ensure t
    :bind
    (("M-x"     . counsel-M-x)
     ("C-x C-f" . counsel-find-file)
     ("C-h f"   . counsel-describe-function)
     ("C-h v"   . counsel-describe-variable))
    )
  )


(use-package popwin
  :ensure t
  :config
  (popwin-mode)
  )


(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts '("j" "k" "l" ";" "u" "i" "o" "p"))
  :bind
  (("C-x o" . switch-window)
   ("C-x 1" . switch-window-then-maximize)
   ("C-x 2" . switch-window-then-split-below)
   ("C-x 3" . switch-window-then-split-right)
   ("C-x 0" . switch-window-then-delete))
  )


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :bind
  (("C-z" . undo)
   ("M-z" . undo-tree-redo))
  )


;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq fci-rule-column 80)
;;   (setq fci-rule-width 1)
;;   (setq fci-rule-color "gray30")

;;   (defun on-off-fci-before-company(command)
;;     "trun ON/OFF the FIC befor company (COMMAND)"
;;     (when (string= "show" command)
;;       (turn-off-fci-mode))
;;     (when (string= "hide" command)
;;       (turn-on-fci-mode)))

;;   (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

;;   (add-hook 'c-mode-hook 'fci-mode)
;;   (add-hook 'c++-mode-hook 'fci-mode)
;;   )


(use-package company
  :ensure t
  :config
  (global-company-mode)
  (add-hook 'gdb-mode-hook             (lambda() (company-mode 0)))
  (add-hook 'eshell-mode-hook          (lambda() (company-mode 0)))
  (add-hook 'shell-mode-hook           (lambda() (company-mode 0)))
  (add-hook 'python-mode-hook          (lambda() (company-mode 0)))
  (add-hook 'inferior-python-mode-hook (lambda() (company-mode 0)))
  (add-hook 'ein:notebook-mode-hook    (lambda() (company-mode 0)))
  )


(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  )


(use-package csv-mode
  :ensure t
  )


(use-package multiple-cursors
  :ensure t
  :bind
  (("C-n" . mc/mark-next-like-this-word)
   ("C-k C-n" . mc/skip-to-next-like-this))
  )


(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-colors '("Pink" "DarkOrange" "yellow" "green" "DodgerBlue1"))
  :bind
  (("C-k C-h" . highlight-symbol-at-point)
   ("C-k C-c" . highlight-symbol-remove-all))
  )


(use-package expand-region
  :ensure t
  :bind
  (("C-d"   . er/expand-region)
   ("C-M-d" . er/contract-region))
  )


(use-package smartparens
  :ensure t
  :init
  (show-paren-mode t)
  (setq-default show-paren-delay 0)
  :config
  (smartparens-global-mode t)
  (defadvice show-paren-function (around fix-show-paren-function activate)
    (cond ((looking-at-p "\\s(") ad-do-it)
          (t (save-excursion
               (ignore-errors (backward-up-list))
               ad-do-it))))
  (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
  )


(use-package avy
  :ensure t
  :bind
  ("C-j" . avy-goto-word-1)
  )


(use-package ag
  :ensure t
  :bind
  ("C-S-f" . ag)
  )


(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  :bind
  (("M-d o" . dumb-jump-go-other-window)
   ("M-d ," . dumb-jump-back))
  )


(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq-default projectile-enable-caching nil)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )


(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )


(use-package neotree
  :ensure t
  :init
  :config
  (setq neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-vc-integration nil
        neo-window-width 40
        neo-theme 'icon
        )

  (defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root (.projectile)"
  (interactive)
  (let ((project-dir
         (ignore-errors
           (projectile-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (if project-dir
            (neotree-dir project-dir)
          (neotree-toggle))
        (if file-name
            (neotree-find file-name))))))
  :bind
  ("M-k M-b" . neotree-project-dir-toggle)
  )


(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  )


(if (display-graphic-p)
    ;; GNU Emacs
    (progn
      (use-package all-the-icons
        :ensure t
        :defer t
        )

      (use-package all-the-icons-dired
        :ensure t
        :after all-the-icons
        :config
        (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
        )

      (use-package all-the-icons-gnus
        :ensure t
        :after all-the-icons
        :config
        (all-the-icons-gnus-setup)
        )

      (use-package spaceline
        :ensure t
        )

      (use-package diminish
        :ensure t
        :config
        (diminish 'flycheck-mode)
        (diminish 'ivy-mode)
        (diminish 'smartparens-mode)
        (diminish 'projectile-mode)
        (diminish 'which-key-mode)
        )

      (use-package spaceline-all-the-icons
        :ensure t
        :after spaceline
        :config
        (spaceline-all-the-icons-theme)
        (spaceline-all-the-icons--setup-neotree)
        (spaceline-all-the-icons--setup-git-ahead)
        (setq spaceline-all-the-icons-separator-type (quote none))
        (spaceline-toggle-all-the-icons-minor-modes-on)
        )

      (use-package all-the-icons-ivy
        :ensure t
        :config (progn (all-the-icons-ivy-setup))
        )

      (use-package doom-themes
        :ensure t
        :config
        ;; (load-theme 'doom-one t)
        (load-theme 'doom-dracula t)
        (doom-themes-visual-bell-config)
        (doom-themes-neotree-config)
        (doom-themes-org-config)
        (setq-default doom-neotree-file-icons t)
        (setq-default nlinum-format "%4d")
        )
      )

  ;; Terminal Emacs
  (use-package material-theme
    :ensure t
    :config
    (enable-theme 'material)
    (setq-default nlinum-format "%4d ")
    )
  )


(provide 'my-packages)
;;; my-packages.el ends here
