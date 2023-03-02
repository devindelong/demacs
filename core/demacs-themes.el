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
;; Custom Treemacs Atom Themes
;;

(require 'treemacs-atom-theme)

;;
;; Kaolin themes
;;

(use-package kaolin-themes
  :straight t)

;;
;; HC Zenburn
;;

(use-package hc-zenburn-theme
  :straight t)

;;
;; Atom
;;

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

  ;; (defvar doom-themes-treemacs-theme "Atom")
  ;; (defvar doom-themes-treemacs-theme "doom-colors")
    (defvar doom-themes-treemacs-theme "doom-atom")

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;
;; Hooks
;;

(defun demacs/init-theme ()
  "Set the theme."
  (load-theme 'doom-sourcerer t))

(add-hook 'emacs-startup-hook #'demacs/init-theme)

(provide 'demacs-themes)

;;; demacs-themes.el ends here.
