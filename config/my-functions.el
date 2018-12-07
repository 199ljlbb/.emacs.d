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


;; Set smooth scroll
(defun smooth-scroll (number-lines increment)
  "Set smmoth scroll (NUMBER-LINES INCREMENT)."
  (if (= 0 number-lines) t
    (progn
      (sit-for 0.02)
      (scroll-up increment)
      (smooth-scroll (- number-lines 1) increment))))

;; Enable mouse scroll
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


;;select a word
(transient-mark-mode 1)
(defun my-syntax-table ()
  "Add '-' & '_' into the syntax table."
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'my-syntax-table)
(add-hook 'text-mode-hook 'my-syntax-table)


(defun select-a-line ()
  "Select a line continuously."
  (interactive)
  (if (not mark-active)
      (set-mark (line-beginning-position))
    (let (start-pos end-pos)
      (setq start-pos (region-beginning)
            end-pos (region-end))
      (deactivate-mark)
      (goto-char start-pos)
      (set-mark (line-beginning-position))
      (goto-char end-pos)
      )
    )
  (end-of-line)
  (forward-char)
  )
(global-set-key (kbd "M-l") 'select-a-line)


(defun toggle-window-split ()
  "Toggle the window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'toggle-window-split)



;; (defun window-toggle-split-direction ()
;;   "Switch window split from horizontally to vertically, or vice versa.

;; i.e. change right window to bottom, or change bottom window to right."
;;   (interactive)
;;   (require 'windmove)
;;   (let ((done))
;;     (dolist (dirs '((right . down) (down . right)))
;;       (unless done
;;         (let* ((win (selected-window))
;;                (nextdir (car dirs))
;;                (neighbour-dir (cdr dirs))
;;                (next-win (windmove-find-other-window nextdir win))
;;                (neighbour1 (windmove-find-other-window neighbour-dir win))
;;                (neighbour2 (if next-win (with-selected-window next-win
;;                                           (windmove-find-other-window neighbour-dir next-win)))))
;;           ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
;;           (setq done (and (eq neighbour1 neighbour2)
;;                           (not (eq (minibuffer-window) next-win))))
;;           (if done
;;               (let* ((other-buf (window-buffer next-win)))
;;                 (delete-window next-win)
;;                 (if (eq nextdir 'right)
;;                     (split-window-vertically)
;;                   (split-window-horizontally))
;;                 (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))


(defun er-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))
(add-hook 'emacs-lisp-mode-hook 'er-remove-elc-on-save)


;; (defun eshell-here ()
;;   "Opens up a new shell in the directory associated with the
;; current buffer's file. The eshell is renamed to match that
;; directory to make multiple eshell windows easier."
;;   (interactive)
;;   (let* ((parent (if (buffer-file-name)
;;                      (file-name-directory (buffer-file-name))
;;                    default-directory))
;;          (height (/ (window-total-height) 3))
;;          (name   (car (last (split-string parent "/" t)))))
;;     (split-window-vertically (- height))
;;     (other-window 1)
;;     (eshell "new")
;;     (rename-buffer (concat "*eshell: " name "*"))

;;     (insert (concat "ls"))
;;     (eshell-send-input)))

;; (global-set-key (kbd "C-`") 'eshell-here)

;; (defun eshell/x ()
;;   (interactive)
;;   (insert "exit")
;;   (eshell-send-input)
;;   (delete-window))

(provide 'my-functions)
;;; my-functions.el ends here
