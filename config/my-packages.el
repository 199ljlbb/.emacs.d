;;; package --- my-packages.el
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


(use-package magit
  :ensure t
  :defer t
  )


(use-package unicode-fonts
  :ensure t
  :commands (unicode-fonts-setup)
  )


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
  :hook (prog-mode . smartparens-mode)
  )


(use-package nlinum
  :ensure t
  :init
  (declare-function global-nlinum-mode "nlinum")
  (declare-function nlinum-mode "nlinum")
  :config
  (setq nlinum-format "%4d")
  :hook (prog-mode . nlinum-mode)
  )


(use-package popwin
  :ensure t
  :init (declare-function popwin-mode "popwin")
  :hook (after-init . popwin-mode)
  )


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (declare-function which-key-mode "which-key")
  :hook (after-init . which-key-mode)
  )


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
  (ein:notebook-mode    . (lambda() (company-mode 0)))
  )


(use-package flycheck
  :ensure t
  :diminish flycheck-mode "Ⓕ"
  :init (declare-function global-flycheck-mode "flycheck")
  :hook (after-init . global-flycheck-mode)
  )


(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  :hook
  (ag-mode . (lambda() (nlinum-mode -1)))
  :bind
  ("C-S-s" . ag)
  )


(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init (declare-function ivy-mode "ivy")
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  )


(use-package swiper
  :ensure t
  :after ivy
  :init (declare-function swiper "swiper")
  :config
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
  :init (declare-function counsel-ag "counsel")
  :config
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
  :init (declare-function global-undo-tree-mode "undo-tree")
  :config (global-undo-tree-mode 1)
  :bind
  ("C-z" . undo)
  ("M-z" . undo-tree-redo)
  )


(use-package csv-mode
  :ensure t
  :defer t
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
  ("C-c p" . projectile-command-map)
  )


(use-package counsel-projectile
  :ensure t
  :config
  (declare-function counsel-projectile-mode "counsel-projectile")
  :hook (after-init . counsel-projectile-mode)
  )


(use-package neotree
  :ensure t
  :defer t
  :init
  (declare-function neotree-dir "neotree")
  (declare-function neotree-hide "neotree")
  (declare-function neotree-find "neotree")
  (declare-function neotree-toggle "neotree")
  :config
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
    :config (setq inhibit-compacting-font-caches t)
    )

  (use-package all-the-icons-dired
    :ensure t
    :defer t
    :config (use-package font-lock+)
    :after all-the-icons
    :hook (dired-mode . all-the-icons-dired-mode)
    )

  (use-package all-the-icons-ivy
    :ensure t
    :after all-the-icons ivy
    :config
    (declare-function all-the-icons-ivy-setup "all-the-icons-ivy")
    (progn (all-the-icons-ivy-setup))
    )

  (use-package spaceline
    :ensure t
    :custom-face (spaceline-highlight-face ((t (:background "white" :foreground "#3E3D31" :inherit (quote mode-line)))))
    )

  (use-package spaceline-all-the-icons
    :ensure t
    :after spaceline all-the-icons
    :init
    (declare-function spaceline-all-the-icons-theme "spaceline-all-the-icons")
    (declare-function spaceline-all-the-icons--setup-neotree "spaceline-all-the-icons")
    (declare-function spaceline-all-the-icons--setup-git-ahead "spaceline-all-the-icons")
    (declare-function spaceline-toggle-all-the-icons-minor-modes-on "spaceline-all-the-icons")
    (declare-function spaceline-toggle-all-the-icons-multiple-cursors "spaceline-all-the-icons")
    :config
    (setq powerline-text-scale-factor 1.0)
    (setq spaceline-all-the-icons-separator-type (quote none))
    (setq spaceline-all-the-icons-icon-set-flycheck-slim (quote solid))
    (setq spaceline-all-the-icons-icon-set-git-ahead (quote commit))
    (setq spaceline-all-the-icons-icon-set-mc (quote pointer))
    (setq spaceline-all-the-icons-icon-set-vc-icon-git (quote git-logo))
    (setq spaceline-all-the-icons-flycheck-alternate t)
    (setq spaceline-all-the-icons-hide-long-buffer-path t)
    (setq spaceline-all-the-icons-highlight-file-name t)
    (setq spaceline-all-the-icons-slim-render t)
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-neotree)
    (spaceline-all-the-icons--setup-git-ahead)
    (spaceline-toggle-all-the-icons-minor-modes-on)
    (spaceline-toggle-all-the-icons-multiple-cursors)
    )

  (use-package highlight-indent-guides
    :ensure t
    :diminish highlight-indent-guides-mode
    :config (setq highlight-indent-guides-method 'character)
    :hook (prog-mode . highlight-indent-guides-mode)
    )

  (use-package doom-themes
    :ensure t
    :init
    (declare-function doom-themes-visual-bell-config "doom-theme")
    (declare-function doom-themes-neotree-config "doom-theme")
    (declare-function doom-themes-org-config "doom-theme")
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
    :config (enable-theme 'material)
    )

  (use-package feebleline
    :ensure t
    :custom
    (feebleline-show-git-branch t)
    (feebleline-show-dir t)
    (feebleline-show-time t)
    (feebleline-show-previous-buffer t)
    :init (declare-function feebleline-mode "feebleline")
    :config (feebleline-mode t)
    )
  )


(cond (is-windows? (gui-mode-config))
      (is-macos? (if (display-graphic-p) (gui-mode-config) (terminal-mode-config)))
      (is-linux? (if (display-graphic-p) (gui-mode-config) (terminal-mode-config)))
      )


(provide 'my-packages)
;;; my-packages.el ends here
