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
;; - go-snippets
;; - gotest
;; - blacken
;; - cc-mode
;; - modern-cpp-font-lock
;; - markdown-mode
;; - swift-mode
;;
;;
;;; Code:


;;
;; Csv
;;

(use-package csv-mode
  :straight t
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;;
;; CUDA Mode
;;

(use-package cuda-mode
  :straight t
  :mode
  ("\\.cu$" . cuda-mode))

;;
;; CMake
;;

(use-package cmake-mode
  :straight t
  :mode
  ("\\.cmake$" . cmake-mode)
  ("CMakeLists.txt$" . cmake-mode))

;;
;; Rust
;;

;; Rust mode
(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save t)
  (lsp-rust-server 'rust-analyzer))

;; Rustic
(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("C-c a t" . rustic-cargo-current-test)
              ("C-c m" . rustic-compile))
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))

;;
;; Go
;;

;; Go mode
(use-package go-mode
  :straight t
  :hook
  (before-save . gofmt-before-save))

;; Go snippets
(use-package go-snippets
  :straight t)

;; Go test
(use-package gotest
  :straight t)
;;
;; Python
;;

;; Blacken
(use-package blacken
  :straight t
  :hook
  (python-mode . blacken-mode))

;;
;; C/C++
;;

;; CC mode
(use-package cc-mode
  :straight t)

;; Modern Cpp fiont lock
(use-package modern-cpp-font-lock
  :straight t)

;;
;; Markdown
;;

;; Markdown mode
(use-package markdown-mode
  :straight t
  :commands gfm-mode markdown-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode))

;;
;; Swift
;;

;; Swift mode
(use-package swift-mode
  :straight t)

;;
;; Provide the package.
;;

(provide 'demacs-languages)

;;; demacs-languages.el ends here
