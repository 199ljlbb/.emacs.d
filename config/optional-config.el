;;; package --- optional-config.el
;;;
;;; Commentary:
;;;   Provide the optional configurations.
;;;
;;; Code:


(defvar is-windows?)
(defvar is-macos?)
(defvar is-linux?)


;; Proxy
;; (setq-default url-proxy-services
;;               '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;                 ("http" . "web-proxy.houston.softwaregrp.net:8080")
;;                 ("https" . "web-proxy.houston.softwaregrp.net:8080")))


;; Font
(let* ((font "Fira Mono")
       (size 9)
       (font-size (format "%s-%s" font size)))
  (setq default-frame-alist `((font . ,font-size)))
  (set-face-attribute 'default t :font font-size))



;; Unicode characters
;; ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ
;; ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ


;;; optional-config.el ends here
