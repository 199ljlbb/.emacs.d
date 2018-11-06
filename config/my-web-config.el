;;; package --- my-web-config.el
;;;
;;; Commentary:
;;;   Provide the web configurations for init.el.
;;;
;;; Code:


(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.html\\'"      . web-mode)
         ("\\.ejs\\'"       . web-mode)
         ("\\.hbs\\'"       . web-mode)
         ("\\.vue\\'"       . web-mode)
         ("\\.css\\'"       . web-mode))
  :config
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (defun my-web-mode-indent-setup ()
    "Setup web mode indent."
    (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
    (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
    (setq web-mode-code-indent-offset 2))  ; web-mode, js code in html file
  (add-hook 'web-mode-hook 'my-web-mode-indent-setup)
  )


(use-package json-mode
  :ensure t
  :defer t
  :mode (("\\.tern-config\\'"  . json-mode)
         ("\\.tern-project\\'" . json-mode))
  )


(provide 'my-web-config)
;;; my-web-config.el ends here
