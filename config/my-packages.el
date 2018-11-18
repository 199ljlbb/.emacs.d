﻿;;; package --- my-packages.el
;;;
;;; Commentary:
;;;   Provide the necessary packages for init.el.
;;;
;;; Code:


(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)


(use-package diminish
  :ensure t
  :diminish abbrev-mode eldoc-mode
  )


(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode)
  )


(use-package magit
  :ensure t
  :defer t
  )


(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup)
  )


(use-package smartparens
  :ensure t
  :init
  (show-paren-mode t)
  (setq-default show-paren-delay 0)
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (defadvice show-paren-function (around fix-show-paren-function activate)
    (cond ((looking-at-p "\\s(") ad-do-it)
          (t (save-excursion
               (ignore-errors (backward-up-list))
               ad-do-it))))
  (sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
  )


(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode)
  (setq nlinum-format "%4d")
  )


(use-package popwin
  :ensure t
  :config
  (popwin-mode)
  )


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  )


(use-package company
  :ensure t
  :diminish company-mode "Ⓒ"
  :config
  (global-company-mode)
  :hook
  (gdb-mode             . (lambda() (company-mode 0)))
  (eshell-mode          . (lambda() (company-mode 0)))
  (shell-mode           . (lambda() (company-mode 0)))
  (python-mode          . (lambda() (company-mode 0)))
  (inferior-python-mode . (lambda() (company-mode 0)))
  (ein:notebook-mode    . (lambda() (company-mode 0)))
  )


(use-package flycheck
  :ensure t
  :diminish flycheck-mode "Ⓕ"
  :config
  (global-flycheck-mode)
  )


(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  :bind
  ("C-S-s" . ag)
  )


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )


(use-package swiper
  :ensure t
  :after ivy
  :init
  (defun swiper-the-region (beg end)
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
  ("C-s" . swiper-the-region)
  )


(use-package counsel
  :ensure t
  :after ivy
  :init
  (defun counsel-ag-the-region (beg end)
    "Counsel ag region or 'empty string' if none highlighted."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil)))
    (if (and beg end)
        (progn
          (deactivate-mark)
          (counsel-ag (buffer-substring-no-properties beg end)))
      (counsel-ag)))

  :bind
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f"  . counsel-describe-function)
  ("<f1> v"  . counsel-describe-variable)
  ("<f1> l"  . counsel-find-library)
  ("<f2> i"  . counsel-info-lookup-symbol)
  ("<f2> u"  . counsel-unicode-char)
  ("C-c k"   . counsel-ag-the-region)
)


(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts '("j" "k" "l" ";" "u" "i" "o" "p"))
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . switch-window-then-maximize)
  ("C-x 2" . switch-window-then-split-below)
  ("C-x 3" . switch-window-then-split-right)
  ("C-x 0" . switch-window-then-delete)
  )


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  :bind
  ("C-z" . undo)
  ("M-z" . undo-tree-redo)
  )


(use-package csv-mode
  :ensure t
  )


(use-package multiple-cursors
  :ensure t
  :bind
  ("C->"     . mc/mark-next-like-this)
  ("C-<"     . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  )


(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-colors '("Pink" "DarkOrange" "yellow" "green" "DodgerBlue1"))
  :bind
  ("C-k C-h" . highlight-symbol-at-point)
  ("C-k C-c" . highlight-symbol-remove-all)
  )


(use-package expand-region
  :ensure t
  :bind
  ("C-d"   . er/expand-region)
  ("C-M-d" . er/contract-region)
  )


(use-package avy
  :ensure t
  :bind
  ("C-j" . avy-goto-word-1)
  )


(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-prefer-searcher 'ag)
  :bind
  ("M-d o" . dumb-jump-go)
  ("M-d ," . dumb-jump-back)
  )


(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  :bind
  ("C-c p" . projectile-command-map)
  )


(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )


(use-package neotree
  :ensure t
  :defer t
  :init
  (defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root (.projectile)"
  (interactive)
  (let ((project-dir
         (ignore-errors (projectile-project-root)))
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
  :hook
  (neo-after-create . (lambda(_unused) (nlinum-mode -1)))
  :bind
  ("M-k M-b" . neotree-project-dir-toggle)
  )


(defun gui-mode-config ()
  "Configurations for all-the-icons."
  (use-package all-the-icons
    :ensure t
    )

  (use-package all-the-icons-dired
    :ensure t
    :defer t
    :init
    (use-package font-lock+)
    :after all-the-icons
    :hook
    (dired-mode . all-the-icons-dired-mode)
    )

  (use-package all-the-icons-gnus
    :ensure t
    :after all-the-icons
    :config
    (all-the-icons-gnus-setup)
    )

  (use-package all-the-icons-ivy
    :ensure t
    :after all-the-icons ivy
    :config (progn (all-the-icons-ivy-setup))
    )

  (use-package spaceline
    :ensure t
    )

  (use-package spaceline-all-the-icons
    :ensure t
    :after spaceline all-the-icons
    :config
    (setq powerline-text-scale-factor 1.0)
    (setq spaceline-all-the-icons-separator-type (quote none))
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-all-the-icons--setup-git-ahead)
    (spaceline-toggle-all-the-icons-minor-modes-on)
    (spaceline-toggle-all-the-icons-multiple-cursors)
    )

  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-dracula t)
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (setq-default doom-neotree-file-icons t)
    )
  )


(defun terminal-mode-config ()
  "Configurations for terminal mode."
  (use-package material-theme
    :ensure t
    :config
    (enable-theme 'material)
    )
  )


(cond (is-windows? (gui-mode-config))
      (is-macos? (if (display-graphic-p) (gui-mode-config) (terminal-mode-config)))
      (is-linux? (if (display-graphic-p) (gui-mode-config) (terminal-mode-config)))
      )


(provide 'my-packages)
;;; my-packages.el ends here
