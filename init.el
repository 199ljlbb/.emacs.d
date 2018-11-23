;;; package --- init.el
;;;
;;; Commentary:
;;;   Emacs initialization file.
;;;
;;; Code:

(defvar is-linux? (eq system-type 'gnu/linux)
  "Is operating system Linux?"
  )

(defvar is-macos? (eq system-type 'darwin)
  "Is operating system Macos?"
  )

(defvar is-windows? (eq system-type 'windows-nt)
  "Is operating system Windows?"
  )

(let ((file-name-handler-alist nil))
  (push "~/.emacs.d/config" load-path)
  (load "optional-config.el")

  (require 'package)
  (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                           ("melpa" . "http://elpa.emacs-china.org/melpa/")))

  (setq package-enable-at-startup nil)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-when-compile (require 'use-package))
  (require 'bind-key)

  (use-package my-key-set)
  (use-package my-packages)
  (use-package my-configurations)
  (use-package my-c-config)
  (use-package my-js-config)
  (use-package my-web-config)
  (use-package my-python-config)
  (use-package my-org-config)
  (use-package my-functions)

  (setq custom-file (expand-file-name "config/custom-file.el" user-emacs-directory))
  (load-file custom-file)
)

(provide 'init)
;;; init.el ends here
