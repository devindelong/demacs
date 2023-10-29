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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight medium :height 155 :width normal :foundry "nil" :family "MesloLGS NF")))))


;;; init.el ends here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "9dccdccfeb236623d5c7cf0250a92308cf307afde4ebdaf173b59e8bbbae1828" "29b4f767c48da68f8f3c2bbf0dde2be58e4ed9c97e685af5a7ab7844f0d08b8b" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "0d0936adf23bba16569c73876991168d0aed969d1e095c3f68d61f87dd7bab9a" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "d19f00fe59f122656f096abbc97f5ba70d489ff731d9fa9437bac2622aaa8b89" "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "ddffe74bc4bf2c332c2c3f67f1b8141ee1de8fd6b7be103ade50abb97fe70f0c" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" default))
 '(size-indication-mode t)
 '(tool-bar-mode nil))
