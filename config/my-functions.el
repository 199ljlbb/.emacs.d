;;; package --- my-functions.el
;;;
;;; Commentary:
;;;   Provide functions for init.el.
;;;
;;; Code:

(defun refresh-file ()
  "Refresh the current file."
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))


;;Set smooth scroll
(defun smooth-scroll (number-lines increment)
  "Set smmoth scroll (NUMBER-LINES INCREMENT)."
  (if (= 0 number-lines) t
    (progn
      (sit-for 0.02)
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

;;Enable mouse scroll
(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 1 -1)))


(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows.  (ARG)."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "M-9") 'transpose-buffers)


;;occur do what I mean
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "C-f") 'occur-dwim)


;;select a word
(transient-mark-mode 1)
(defun my-syntax-table ()
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'my-syntax-table)
(add-hook 'text-mode-hook 'my-syntax-table)


(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-`") 'eshell-here)

(defun eshell/x ()
  (interactive)
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(provide 'my-functions)
;;; my-functions.el ends here
