;;; package --- my-js-config.el
;;;
;;; Commentary:
;;;   Provide the web configurations for init.el.
;;;
;;; Code:


(use-package js2-mode
  :ensure t
  :mode
   ("\\.js\\'" . js2-mode)
   )


(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  )


(use-package indium
  :ensure t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  )


(provide 'my-js-config)
;;; my-js-config.el ends here
