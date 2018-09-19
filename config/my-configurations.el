;;; package --- my-configurations.el
;;;
;;; Commentary:
;;;   Provide the configurations for init.el.
;;;
;;; Code:

(setenv "PATH" "/usr/local/bin:$PATH" t)
(push "/usr/local/bin" exec-path)

;; automatically reload the modifications
(global-auto-revert-mode t)

;;startup with fullscreen
;; (set-frame-parameter nil 'fullscreen 'fullboth)

;;Disable startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;Disable menu bar
(menu-bar-mode -1)

;;Disable tool bar
(tool-bar-mode -1)

;;Disable scroll bar
(when (display-graphic-p)
  (progn
    (scroll-bar-mode -1)))

;;Disable backup file
(setq make-backup-files nil)

;;Disable auto save
(setq auto-save-default nil)

;;Enable rewrite the selection region without using delete key
(delete-selection-mode t)

;;Show whitespace
;; (global-whitespace-mode 1)
;; (unless (display-graphic-p)
;;   (setq-default whitespace-space-regexp "\\(\u3000\\)")
;;   (setq-default whitespace-display-mappings ()))
;; (setq-default whitespace-style '(face tabs tab-mark spaces space-mark))
;;Show whitespace of trailing
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(defun disable-trailing-mode-hook ()
  "Disable show tail whitespace."
  (setq show-trailing-whitespace nil)
  )

(defvar disable-trailing-modes
  '(comint-mode
    eshell-mode
    shell-mode
    eww-mode
    ein:notebook-mode
    ein:notebooklist-mode
    ))

(mapc (lambda (mode)
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  'disable-trailing-mode-hook))
      disable-trailing-modes)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)

;;Sow cloumn number (on the status bar)
(setq column-number-mode t)

;;Highlight the current line
(global-hl-line-mode 1)

;;Set TAB
(setq-default default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;Enable to use shift to select the text
(setq shift-select-mode t)

;;Set xterm-mode-mode ture under terminal
(when (eq window-system nil)
  (xterm-mouse-mode t))

;;Scroll by 1 line
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1
      mouse-wheel-scroll-amount (quote (0.01)))

(provide 'my-configurations)
;;; my-configurations.el ends here
