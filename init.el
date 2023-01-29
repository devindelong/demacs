;;; init.el --- Emacs Init.

;;; Commentary:
;;; This must come before configurations of installed packages. Don't delete
;;; this line. If you don't want it, just comment it out by adding a semicolon
;;; to the start of the line. You may delete these explanatory comments.

;;; Code:

;; ----------------------------------------------------------------------------
;;
;; Configure garbage collection
;;
;; ----------------------------------------------------------------------------

;; Following Doom-Emacs FAQ, we max the garbage collection threshold on startup,
;; and reset it to the original value after.

;; Max memory available for gc on startup
(defvar demacs/gc-cons-threshold 46777216)

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
  (expand-file-name "demacs" user-emacs-directory))
(add-to-list 'load-path demacs-dir)

(require 'demacs-general)
(require 'demacs-org)
(require 'demacs-system)
(require 'demacs-ui)
(require 'demacs-editor)
(require 'demacs-languages)
(require 'demacs-git)


;; -----------------------------------------------------------------------------
;;
;; Emacs Inserted
;;
;; -----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "570263442ce6735821600ec74a9b032bc5512ed4539faf61168f2fdf747e0668" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(zenburn-theme use-package unicode-fonts spaceline rustic rainbow-delimiters projectile perspective mood-line magit-libgit magit-delta gotest go-snippets forge flycheck-posframe exec-path-from-shell esup elm-mode doom-themes dimmer diminish diff-hl company-prescient company-posframe cmake-mode blacken bar-cursor auto-indent-mode))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "MesloLGS NF")))))


;;; init.el ends here.
