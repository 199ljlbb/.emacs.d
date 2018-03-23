;;; package --- my-key-set.el
;;;
;;; Commentary:
;;;   Unset key for init.el.
;;;
;;; Code:

(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-p"))
(global-unset-key (kbd "C-f"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "C-n"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-r"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "C-u"))

(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-d"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-z"))


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
          ;; shift
          (if (= x 2) (setq tkey "S-"))
          ;; alt
          (if (= x 3) (setq tkey "M-"))
          ;; alt + shift
          (if (= x 4) (setq tkey "M-S-"))
          ;; ctrl
          (if (= x 5) (setq tkey "C-"))
          ;; ctrl + shift
          (if (= x 6) (setq tkey "C-S-"))
          ;; ctrl + alt
          (if (= x 7) (setq tkey "C-M-"))
          ;; ctrl + alt + shift
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
