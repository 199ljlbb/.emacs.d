;;; package --- my-c-config.el
;;;
;;; Commentary:
;;;   Provide the C/C++/OC configurations for init.el.
;;;
;;; Code:


(use-package cc-mode
  :ensure t
  :config
  (define-key c-mode-base-map (kbd "C-d") nil)

  ;; (defface font-lock-function-call-face '((t (:foreground "HotPink3")))
  ;;   "Font Lock mode face used to highlight function calls."
  ;;   :group 'font-lock-highlighting-faces)

  ;; (add-hook 'c-mode-hook
  ;;           (lambda ()
  ;;             (font-lock-add-keywords
  ;;              nil
  ;;              '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-call-face)) t)))
  ;; (add-hook 'c++-mode-hook
  ;;           (lambda ()
  ;;             (font-lock-add-keywords
  ;;              nil
  ;;              '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-call-face)) t)))
  ;; (add-hook 'objc-mode-hook
  ;;           (lambda ()
  ;;             (font-lock-add-keywords
  ;;              nil
  ;;              '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-call-face)) t)))
  )


(use-package google-c-style
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  )


(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt|.cmake"
  )


(use-package lsp-mode
  :ensure t
  )


(use-package cquery
  :ensure t
  :config
  (setq cquery-executable "C:\\Users\\Thinking\\cquery\\build\\Release\\bin\\cquery.exe")
  )


;; (use-package rtags
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq rtags-path "/usr/local/bin")
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   :bind
;;   (:map c-mode-base-map
;;         ("M-d M-d" . rtags-find-symbol-at-point)
;;         ("M-d M-r" . rtags-find-references-at-point)
;;         ("M-,"     . rtags-location-stack-back)
;;         ("M-."     . rtags-location-stack-forward)
;;         ("M-c"     . company-rtags)
;;         )
;;   )


;; (use-package ivy-rtags
;;   :ensure t
;;   :config
;;   (setq rtags-display-result-backend 'ivy)
;;   )


;; (use-package flycheck-rtags
;;   :ensure t
;;   :config
;;   (defun my-flycheck-rtags-setup ()
;;     "Setup flycheck-rtags."
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil)
;;     (setq-local flycheck-check-syntax-automatically nil))
;;   (add-hook 'c-mode-hook    #'my-flycheck-rtags-setup)
;;   (add-hook 'c++-mode-hook  #'my-flycheck-rtags-setup)
;;   (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
;;   )


;; (use-package irony
;;   :ensure t
;;   :config
;;   (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))

;;   (add-hook 'c++-mode-hook  'irony-mode)
;;   (add-hook 'c-mode-hook    'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)

;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


;;   ;; For MACOS using command
;;   ;; echo | clang -x c++ -v -E - 2>&1 | sed -n '/^#include </,/^End/s|^[^/]*\([^ ]*/include[^ ]*\).*$|-I\1|p'
;;   ;; to get the variable: irony-additonal-clang-options
;;   (custom-set-variables
;;    '(irony-additional-clang-options
;;      '("-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1"
;;        "-I/usr/local/include"
;;        "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/8.0.0/include"
;;        "-I/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include"
;;        "-I/usr/include")))
;;   )


;; (use-package flycheck-irony
;;   :ensure t
;;   :after irony
;;   :config
;;   (eval-after-load 'flycheck
;;     '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;;   )

;; (use-package company-irony
;;   :ensure t
;;   :after irony
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony))
;;   )

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list
;;       'company-backends '(company-irony-c-headers company-irony)))
;;   )


;; (setq-default tags-table-list nil)
;; (global-set-key (kbd "M-k M-d") (function xref-find-definitions-other-window))


(provide 'my-c-config)
;;; my-c-config.el ends here
