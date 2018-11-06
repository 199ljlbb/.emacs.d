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


(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-auto-revert-mode t) ;; Automatically reload the modifications
(delete-selection-mode t) ;; Enable rewrite the selection region without using delete key
(global-hl-line-mode t)
(if (not is-windows?) (display-graphic-p) (progn (scroll-bar-mode -1)))

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







;; (let* ((font "Fira Mono")
;;        (size 9)
;;        (font-size (format "%s-%s" font size)))
;;   (setq default-frame-alist `((font . ,font-size)))
;;   (set-face-attribute 'default t :font font-size))

;; (defun hook-if-daemonp (func)
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions
;;                 (lambda (frame)
;;                   (with-selected-frame frame
;;                     (funcall func))))
;;     (funcall func)))

;; (hook-if-daemonp
;;  (lambda ()
;;    (when (display-graphic-p)
;;      (let ((utf8-font "Fira Code"))
;;        (set-fontset-font "fontset-startup" '(#x000000 . #x3FFFFF) utf8-font)
;;        (set-fontset-font "fontset-default" '(#x000000 . #x3FFFFF) utf8-font)
;;        (set-fontset-font "fontset-standard" '(#x000000 . #x3FFFFF) utf8-font)))))







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


(provide 'my-configurations)
;;; my-configurations.el ends here
