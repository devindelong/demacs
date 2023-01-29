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

;;
;; Kaolin themes
;;

(use-package kaolin-themes
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

  (defvar doom-themes-treemacs-theme "doom-colors")

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun demacs/init-theme ()
  (load-theme 'doom-opera t))

(add-hook 'emacs-startup-hook #'demacs/init-theme)

(provide 'demacs-themes)

;;; demacs-themes.el ends here.
