;;; package --- python-config.el
;;;
;;; Commentary:
;;;   Provide the python configurations for init.el.
;;;
;;; Code:


(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp-deferred))))


(use-package dap-python
  :after dap-mode)


(provide 'python-config)
;;; python-config.el ends here
