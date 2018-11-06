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

(when is-windows?
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect)
  (setq inhibit-compacting-font-caches t)
  )

;; (setq-default url-proxy-services
;;               '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;                 ("http" . "web-proxy.houston.softwaregrp.net:8080")
;;                 ("https" . "web-proxy.houston.softwaregrp.net:8080")))

(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

(setq package-enable-at-startup nil)
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


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
