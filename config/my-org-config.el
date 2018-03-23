;;; package --- my-org-config.el
;;;
;;; Commentary:
;;;   Emacs initialization file.
;;;
;;; Code:


(use-package org
  :ensure t
  :config
  (setq org-src-fontify-natively t)
  )


(use-package org-bullets
  :ensure t
  :init
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )


(provide 'my-org-config)
;;; my-org-config.el ends here
