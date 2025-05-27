;;; demacs-languages.el --- Language support configuration.
;;
;;; Commentary:
;;
;; This file contains packages and general support for various programming
;; languages.
;;
;;
;;; Packages:
;;
;; - csv-mode
;; - cuda-mode
;; - cmake-mode
;; - rust-mode
;; - rustic
;; - go-mode
;; - gotest
;; - blacken
;; - cc-mode
;; - modern-cpp-font-lock
;; - markdown-mode
;; - swift-mode
;;
;;
;;; Code:


(use-package yaml-mode
  :straight t
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)
         ("\\.clangd\\'" . yaml-mode)
         ("\\.clang-format\\'" . yaml-mode)
         ("\\.circleci/config\\.yml\\'" . yaml-mode)))

(use-package protobuf-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package csv-mode
  :straight t
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))


(use-package cuda-mode
  :straight t
  :mode
  ("\\.cu$" . cuda-mode))


(use-package cmake-mode
  :straight t
  :mode
  ("\\.cmake$" . cmake-mode)
  ("CMakeLists.txt$" . cmake-mode))


(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save t)
  (lsp-rust-server 'rust-analyzer))


(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("C-c a t" . rustic-cargo-current-test)
              ("C-c m" . rustic-compile))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))


(use-package go-mode
  :straight t
  :hook
  (before-save . gofmt-before-save))


(use-package gotest
  :straight t)


(use-package blacken
  :straight t
  :hook
  (python-mode . blacken-mode))


(use-package cc-mode
  :straight t)


(use-package modern-cpp-font-lock
  :straight t)


(use-package markdown-mode
  :straight t
  :commands gfm-mode markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode))


(use-package swift-mode
  :straight t)


(provide 'demacs-languages)

;;; demacs-languages.el ends here
