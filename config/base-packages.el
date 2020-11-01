;;; package --- base-packages.el
;;;
;;; Commentary:
;;;   Provide the base packages for init.el.
;;;
;;; Code:


(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Thinking in Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 10))))


(use-package diminish
  :ensure t
  :diminish abbrev-mode eldoc-mode)


(use-package async
  :ensure t
  :config (async-bytecomp-package-mode 1)
  :commands (async-start))


(use-package unicode-fonts
  :ensure t
  :commands (unicode-fonts-setup))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (declare-function which-key-mode "which-key")
  :bind* ("M-m ?" . which-key-show-top-level)
  :hook (after-init . which-key-mode))


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


(use-package avy
  :ensure t
  :bind ("C-j" . avy-goto-word-1))


(use-package undo-tree
  :ensure t
  :init (declare-function global-undo-tree-mode "undo-tree")
  :config (global-undo-tree-mode 1)
  :bind
  ("C-z" . undo)
  ("M-z" . undo-tree-redo))


(use-package expand-region
  :ensure t
  :bind
  ("C-d"   . er/expand-region)
  ("C-M-d" . er/contract-region))


(use-package multiple-cursors
  :ensure t
  :bind
  ("C->"     . mc/mark-next-like-this)
  ("C-<"     . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))


(use-package highlight-symbol
  :ensure t
  :init
  (defvar highlight-symbol-list)
  (declare-function highlight-symbol-symbol-highlighted-p "highlight-symbol-symbol-highlighted-p")
  (declare-function highlight-symbol-add-symbol-with-face "highlight-symbol-add-symbol-with-face")
  (declare-function highlight-symbol-foreground-color "highlight-symbol-foreground-color")
  (declare-function highlight-symbol-next-color "highlight-symbol-next-color")
  (declare-function highlight-symbol-mode-remove-temp "highlight-symbol-mode-remove-temp")
  (eval-after-load "highlight-symbol"
    '(defun highlight-symbol-add-symbol (symbol &optional color)
       "Override this function for support convenience set foreground and background"
       (unless (highlight-symbol-symbol-highlighted-p symbol)
         (when (equal symbol highlight-symbol)
           (highlight-symbol-mode-remove-temp))
         (let ((color (or color (highlight-symbol-next-color)))
               f-color b-color)
           (unless (facep color)
             (if (consp color)
                 (setq f-color (car color)
                       b-color (cdr color))
               (setq f-color highlight-symbol-foreground-color
                     b-color color))
             (setq color `((background-color . ,b-color)
                           (foreground-color . ,f-color))))
           ;; highlight
           (highlight-symbol-add-symbol-with-face symbol color)
           (push symbol highlight-symbol-list)))))
  :config
  (setq highlight-symbol-colors
        '(("white" . "Red")
          ("black" . "Yellow")
          ("black" . "DeepPink")
          ("black" . "Cyan")
          ("white" . "MediumPurple1")
          ("black" . "SpringGreen1")
          ("white" . "DarkOrange")
          ("white" . "HotPink1")
          ("black" . "RoyalBlue1")
          ("white" . "OliveDrab")))
  :bind
  ("C-k C-h" . highlight-symbol-at-point)
  ("C-k C-c" . highlight-symbol-remove-all))


(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  :hook (ag-mode . (lambda() (nlinum-mode -1)))
  :bind ("C-S-s" . ag))


(use-package ivy
  :ensure t
  :defer t
  :diminish ivy-mode
  :init (declare-function ivy-mode "ivy")
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))


(use-package ivy-posframe
  :ensure t
  :after ivy
  :diminish ivy-posframe-mode
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-frame-center)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-frame-center)
          (t               . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))


(use-package swiper
  :ensure t
  :defer t
  :after ivy
  :init
  (declare-function swiper "swiper")
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
  ("C-s" . swiper-the-region))


(use-package counsel
  :ensure t
  :defer t
  :after ivy
  :init
  (declare-function counsel-ag "counsel")
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
  ("C-c k"   . counsel-ag-the-region))


(provide 'base-packages)
;;; base-packages.el ends here
