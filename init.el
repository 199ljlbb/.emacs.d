;;; package --- init.el
;;;
;;; Commentary:
;;;   Emacs initialization file.
;;;
;;; Code:

(setq-default gc-cons-threshold 104857600)

(require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(push "~/.emacs.d/config" load-path)
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

(provide 'init)
;;; init.el ends here
