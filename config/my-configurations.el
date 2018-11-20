;;; package --- my-configurations.el
;;;
;;; Commentary:
;;;   Provide the configurations for init.el.
;;;
;;; Code:

(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)

(if (or is-linux? is-macos?)
  (progn
    (setenv "PATH" "/usr/local/bin:$PATH" t)
    (push "/usr/local/bin" exec-path)
    )
  )


(menu-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode t) ;; Automatically reload the modifications
(delete-selection-mode t) ;; Enable rewrite the selection region without using delete key
(global-hl-line-mode t)


(when (display-graphic-p) (toggle-scroll-bar -1))

(defun disable-scroll-bar (frame)
  "Disable scroll bar (on a FRAME)."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'disable-scroll-bar)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq shift-select-mode t)
(setq-default default-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;;Scroll by 1 line
(setq scroll-conservatively 10000
      scroll-margin 1
      scroll-step 1
      )
(when is-macos? (setq mouse-wheel-scroll-amount (quote (0.01))))

;;Set xterm-mode-mode ture under terminal
(when (eq window-system nil) (xterm-mouse-mode t))


(setq-default show-trailing-whitespace t) ;;Show whitespace of trailing
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
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;; (custom-set-faces
;;  '(spaceline-highlight-face ((t (:background "white" :foreground "#3E3D31" :inherit (quote mode-line)))))
;;  )




(provide 'my-configurations)
;;; my-configurations.el ends here
