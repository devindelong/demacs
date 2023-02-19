;;; init.el --- Emacs Init.
;;
;;; Commentary:
;;
;;  This must come before configurations of installed packages. Don't delete
;;  this line. If you don't want it, just comment it out by adding a semicolon
;;  to the start of the line. You may delete these explanatory comments.
;;
;;; Code:
;;

;; ----------------------------------------------------------------------------
;;
;; Configure garbage collection
;;
;; ----------------------------------------------------------------------------

;; Following Doom-Emacs FAQ, we max the garbage collection threshold on startup,
;; and reset it to the original value after.

;; Max memory available for gc on startup
(defvar demacs/gc-cons-threshold 1073741824)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold demacs/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun demacs/defer-garbage-collection-h ()
  "Max memory available for gc when opening minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun demacs/restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
    1 nil (lambda () (setq gc-cons-threshold demacs/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'demacs/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'demacs/restore-garbage-collection-h)

(setq garbage-collection-messages t)


;; ----------------------------------------------------------------------------
;;
;; Define and initialize package repositories
;;
;; ----------------------------------------------------------------------------

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(defvar straight-check-for-modifications '(check-on-save find-when-checking))

;; This preamble is part of straight-use-package My understanding, in
;; reading straight documentation is that it has better load
;; times. However, the configuration options I often see leverage
;; "use-package" which is why most of my package declarations look as
;; they do.
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; I saw that straight loaded use-package to take advantage of the
;; use-package syntax which is often how things are documented.
(straight-use-package 'use-package)
(defvar use-package-always-ensure t)

;; Package repos
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; Emacs needs ispell on macos. Installed aspell via Homebrew.
(setq ispell-program-name "aspell")

;;Disable cl package deprecates warnings.
(setq byte-compile-warnings '(cl-functions))

;; ----------------------------------------------------------------------------
;;
;; Configure project directories.
;;
;; ----------------------------------------------------------------------------

;; Emacs user directory.
(defvar emacs-dir (expand-file-name user-emacs-directory))

;; ;; Define the init file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; Settings
(defvar demacs-dir
  (expand-file-name "demacs/core" user-emacs-directory))
(add-to-list 'load-path demacs-dir)

(require 'demacs-general)
(require 'demacs-org)
(require 'demacs-system)
(require 'demacs-ui)
(require 'demacs-themes)
(require 'demacs-editor)
(require 'demacs-languages)
(require 'demacs-git)

;; -----------------------------------------------------------------------------
;;
;; Emacs Inserted
;;
;; -----------------------------------------------------------------------------

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "MesloLGS NF")))))


;;; init.el ends here.
