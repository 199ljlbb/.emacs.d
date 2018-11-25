;;; package --- my-key-set.el
;;;
;;; Commentary:
;;;   Unset key for init.el.
;;;
;;; Code:


(defvar is-linux?)
(defvar is-macos?)
(defvar is-windows?)

;; Replace <ESC> to "C-g" except terminal Emacs in MaxOS
(if is-macos? (if (display-graphic-p) (define-key key-translation-map (kbd "ESC")(kbd "C-g")))
  (define-key key-translation-map (kbd "ESC")(kbd "C-g"))
  )

;; Unset <Control> key set
(global-unset-key (kbd "C-0"))
(global-unset-key (kbd "C-1"))
(global-unset-key (kbd "C-2"))
(global-unset-key (kbd "C-3"))
(global-unset-key (kbd "C-4"))
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))
(global-unset-key (kbd "C-SPC"))

(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-u"))
(global-unset-key (kbd "C-o"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "C-u"))
(global-unset-key (kbd "C--"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-\\"))

;; Unset <Meta> key set
(global-unset-key (kbd "M-0"))
(global-unset-key (kbd "M-1"))
(global-unset-key (kbd "M-2"))
(global-unset-key (kbd "M-3"))
(global-unset-key (kbd "M-4"))
(global-unset-key (kbd "M-5"))
(global-unset-key (kbd "M-6"))
(global-unset-key (kbd "M-7"))
(global-unset-key (kbd "M-8"))
(global-unset-key (kbd "M-9"))

(global-unset-key (kbd "M-a"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-q"))
(global-unset-key (kbd "M-r"))
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-v"))
(global-unset-key (kbd "M-y"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-DEL"))
(global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "M-@"))
(global-unset-key (kbd "M-$"))
(global-unset-key (kbd "M-%"))
(global-unset-key (kbd "M-&"))
(global-unset-key (kbd "M-^"))
(global-unset-key (kbd "M-'"))
(global-unset-key (kbd "M-("))
(global-unset-key (kbd "M-)"))
(global-unset-key (kbd "M--"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-/"))
(global-unset-key (kbd "M-\\"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-|"))
(global-unset-key (kbd "M-~"))
(global-unset-key (kbd "M-`"))

;; Unset <Control + Meta> key set
(global-unset-key (kbd "C-M-SPC"))
(global-unset-key (kbd "C-M-%"))
(global-unset-key (kbd "C-M--"))
(global-unset-key (kbd "C-M-."))
(global-unset-key (kbd "C-M-/"))
(global-unset-key (kbd "C-M-9"))
(global-unset-key (kbd "C-M-@"))
(global-unset-key (kbd "C-M-\\"))
(global-unset-key (kbd "C-M-a"))
(global-unset-key (kbd "C-M-b"))
(global-unset-key (kbd "C-M-c"))
(global-unset-key (kbd "C-M-d"))
(global-unset-key (kbd "C-M-e"))
(global-unset-key (kbd "C-M-f"))
(global-unset-key (kbd "C-M-h"))
(global-unset-key (kbd "C-M-i"))
(global-unset-key (kbd "C-M-j"))
(global-unset-key (kbd "C-M-k"))
(global-unset-key (kbd "C-M-l"))
(global-unset-key (kbd "C-M-n"))
(global-unset-key (kbd "C-M-o"))
(global-unset-key (kbd "C-M-p"))
(global-unset-key (kbd "C-M-q"))
(global-unset-key (kbd "C-M-r"))
(global-unset-key (kbd "C-M-s"))
(global-unset-key (kbd "C-M-t"))
(global-unset-key (kbd "C-M-u"))
(global-unset-key (kbd "C-M-v"))
(global-unset-key (kbd "C-M-w"))
(global-unset-key (kbd "C-M-x"))
(global-unset-key (kbd "C-M-0"))
(global-unset-key (kbd "C-M-1"))
(global-unset-key (kbd "C-M-2"))
(global-unset-key (kbd "C-M-3"))
(global-unset-key (kbd "C-M-4"))
(global-unset-key (kbd "C-M-5"))
(global-unset-key (kbd "C-M-6"))
(global-unset-key (kbd "C-M-7"))
(global-unset-key (kbd "C-M-8"))

;; Unset <Control + Meta + Shift> key set
(global-unset-key (kbd "C-M-S-v"))


;; (define-key ggtags-mode-map (kbd "M-.") nil)
;; (define-key ggtags-mode-map (kbd "M-,") nil)
;; (define-key ggtags-mode-map (kbd "M-d") nil)
;; (define-key ggtags-mode-map (kbd "M-]") nil)
;; (define-key ggtags-mode-map (kbd "C-M-.") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-f") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-j") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-?") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-o") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-n") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-h") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-b") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-k") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-DEL") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-i") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-p") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-g") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-%") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-SPC") nil)
;; (define-key ggtags-mode-map (kbd "C-c M-/") nil)


(if (getenv "TMUX")
    (progn
      (let ((x 2) (tkey ""))
        (while (<= x 8)
          (if (= x 2) (setq tkey "S-"))
          (if (= x 3) (setq tkey "M-"))
          (if (= x 4) (setq tkey "M-S-"))
          (if (= x 5) (setq tkey "C-"))
          (if (= x 6) (setq tkey "C-S-"))
          (if (= x 7) (setq tkey "C-M-"))
          (if (= x 8) (setq tkey "C-M-S-"))

          ;; arrows
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d A" x))
            (kbd (format "%s<up>" tkey)))

          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d B" x))
            (kbd (format "%s<down>" tkey)))

          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d C" x))
            (kbd (format "%s<right>" tkey)))

          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d D" x))
            (kbd (format "%s<left>" tkey)))

          ;; home
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d H" x))
            (kbd (format "%s<home>" tkey)))

          ;; end
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d F" x))
            (kbd (format "%s<end>" tkey)))

          ;; page up
          (define-key key-translation-map
            (kbd (format "M-[ 5 ; %d ~" x))
            (kbd (format "%s<prior>" tkey)))

          ;; page down
          (define-key key-translation-map
            (kbd (format "M-[ 6 ; %d ~" x))
            (kbd (format "%s<next>" tkey)))

          ;; insert
          (define-key key-translation-map
            (kbd (format "M-[ 2 ; %d ~" x))
            (kbd (format "%s<delete>" tkey)))

          ;; delete
          (define-key key-translation-map
            (kbd (format "M-[ 3 ; %d ~" x))
            (kbd (format "%s<delete>" tkey)))

          ;; f1
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d P" x))
            (kbd (format "%s<f1>" tkey)))

          ;; f2
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d Q" x))
            (kbd (format "%s<f2>" tkey)))

          ;; f3
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d R" x))
            (kbd (format "%s<f3>" tkey)))

          ;; f4
          (define-key key-translation-map
            (kbd (format "M-[ 1 ; %d S" x))
            (kbd (format "%s<f4>" tkey)))

          ;; f5
          (define-key key-translation-map
            (kbd (format "M-[ 15 ; %d ~" x))
            (kbd (format "%s<f5>" tkey)))

          ;; f6
          (define-key key-translation-map
            (kbd (format "M-[ 17 ; %d ~" x))
            (kbd (format "%s<f6>" tkey)))

          ;; f7
          (define-key key-translation-map
            (kbd (format "M-[ 18 ; %d ~" x))
            (kbd (format "%s<f7>" tkey)))

          ;; f8
          (define-key key-translation-map
            (kbd (format "M-[ 19 ; %d ~" x))
            (kbd (format "%s<f8>" tkey)))

          ;; f9
          (define-key key-translation-map
            (kbd (format "M-[ 20 ; %d ~" x))
            (kbd (format "%s<f9>" tkey)))

          ;; f10
          (define-key key-translation-map
            (kbd (format "M-[ 21 ; %d ~" x))
            (kbd (format "%s<f10>" tkey)))

          ;; f11
          (define-key key-translation-map
            (kbd (format "M-[ 23 ; %d ~" x))
            (kbd (format "%s<f11>" tkey)))

          ;; f12
          (define-key key-translation-map
            (kbd (format "M-[ 24 ; %d ~" x))
            (kbd (format "%s<f12>" tkey)))

          ;; f13
          (define-key key-translation-map
            (kbd (format "M-[ 25 ; %d ~" x))
            (kbd (format "%s<f13>" tkey)))

          ;; f14
          (define-key key-translation-map
            (kbd (format "M-[ 26 ; %d ~" x))
            (kbd (format "%s<f14>" tkey)))

          ;; f15
          (define-key key-translation-map
            (kbd (format "M-[ 28 ; %d ~" x))
            (kbd (format "%s<f15>" tkey)))

          ;; f16
          (define-key key-translation-map
            (kbd (format "M-[ 29 ; %d ~" x))
            (kbd (format "%s<f16>" tkey)))

          ;; f17
          (define-key key-translation-map
            (kbd (format "M-[ 31 ; %d ~" x))
            (kbd (format "%s<f17>" tkey)))

          ;; f18
          (define-key key-translation-map
            (kbd (format "M-[ 32 ; %d ~" x))
            (kbd (format "%s<f18>" tkey)))

          ;; f19
          (define-key key-translation-map
            (kbd (format "M-[ 33 ; %d ~" x))
            (kbd (format "%s<f19>" tkey)))

          ;; f20
          (define-key key-translation-map
            (kbd (format "M-[ 34 ; %d ~" x))
            (kbd (format "%s<f20>" tkey)))

          (setq x (+ x 1))
          ))
      )
  )



(provide 'my-key-set)
;;; my-key-set.el ends here
