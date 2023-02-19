;;; demacs-themes.el --- Theme configuration.
;;
;;; Commentary:
;;
;; This file contains theme configurations.
;;
;;
;;; Packages:
;;
;; - kaolin-themes
;; - doom-themes
;;
;;; Code:


;;
;; Kaolin themes
;;
(require 'treemacs-atom-theme)

(use-package kaolin-themes
  :straight t)

(use-package hc-zenburn-theme
  :straight t)

(use-package atom-one-dark-theme
  :straight t)

;;
;; Doom themes
;;

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (defvar doom-themes-treemacs-theme "Atom")

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun demacs/init-theme ()
  "Set the theme."
  (load-theme 'doom-one t))

(add-hook 'emacs-startup-hook #'demacs/init-theme)

(provide 'demacs-themes)

;;; demacs-themes.el ends here.
