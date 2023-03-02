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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" default)))
