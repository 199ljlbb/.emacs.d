;;; package --- theme.el
;;;
;;; Commentary:
;;;   Configurations of themes.
;;;
;;; Code:


(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)


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
  (if (display-graphic-p) (setq-default doom-neotree-file-icons t)))


(defun all-the-icons-config ()
  "Configuration of all-the-icons related packages."
  (use-package all-the-icons
    :ensure t
    :config (setq inhibit-compacting-font-caches t))

  (use-package all-the-icons-dired
    :ensure t
    :defer t
    :config (use-package font-lock+)
    :after all-the-icons
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package all-the-icons-ivy
    :ensure t
    :after all-the-icons ivy
    :init (declare-function all-the-icons-ivy-setup "all-the-icons-ivy")
    :config (progn (all-the-icons-ivy-setup)))
  )


(defun spaceline-config ()
  "Configuration of GUI mode."
  (all-the-icons-config)
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
  )


(defun doom-modeline-config ()
  "Configurations of terminal mode."
  (use-package doom-modeline
    :ensure t
    :defer t
    :hook (after-init . doom-modeline-mode)
    :config
    (setq inhibit-compacting-font-caches t)
    (setq doom-modeline-height 10)
    (setq doom-modeline-bar-width 3)
    (setq doom-modeline-minor-modes t)
    (setq doom-modeline-buffer-file-name-style 'auto)
    (setq doom-modeline-window-width-limit fill-column)
    (setq doom-modeline-project-detection 'project)
    (setq doom-modeline-icon (display-graphic-p))
    (setq doom-modeline-major-mode-icon t)
    (setq doom-modeline-major-mode-color-icon t)
    (setq doom-modeline-buffer-modification-icon t)
    (setq doom-modeline-lsp t)
    (setq doom-modeline-env-version t)
    )
  )


(cond (is-windows? (if (display-graphic-p) (progn (all-the-icons-config) (doom-modeline-config)) (spaceline-config)))
      (is-macos? (if (display-graphic-p) (spaceline-config) (doom-modeline-config)))
      (is-linux? (if (display-graphic-p) (spaceline-config) (doom-modeline-config)))
      )


(provide 'theme)
;;; theme.el ends here
