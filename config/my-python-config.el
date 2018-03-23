;;; package --- my-python-config.el
;;;
;;; Commentary:
;;;   Provide the python configurations for init.el.
;;;
;;; Code:


(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc--backend-python-command "jedi")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  )


(use-package ein
  :ensure t
  :config
  (setq-default ein:jupyter-default-server-command "/usr/local/bin/jupyter")
  (setq-default ein:jupyter-default-notebook-directory "~/")
  )


(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )


(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq-default jedi:complete-on-dot t)
  )


(provide 'my-python-config)
;;; my-python-config.el ends here
