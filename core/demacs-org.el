;;; demacs-org.el --- Org configuration.
;;
;;; Commentary:
;;
;; In this section, Org mode and related packages are configured
;;
;;; Packages:
;;
;; - org
;; - org-bullets
;; - toc-org
;; - org-pomodoro
;;
;;; Code:


(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'org-mouse) ;; Org mouse for checkboxes
  (setq org-agenda-files (list "~/org/agenda.org")
        org-support-shift-select t
        org-hide-emphasis-markers t  ;; Hide markers for italic, bold etc
        org-log-done 'time           ;; Print timestamp when done
        org-src-tab-acts-natively t  ;; Handle source blocks
        org-hide-leading-stars t
        org-latex-listings 'minted   ;; Make LaTex use minted when exporting
        org-latex-pdf-process
        '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook
  (org-mode . toggle-word-wrap)
  (org-mode . toggle-truncate-lines))


(use-package org-bullets
  :straight t
  :after org
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))


(use-package toc-org
  :straight t
  :after org
  :hook
  (org-mode . toc-org-mode))


(use-package org-pomodoro
  :straight t
  :after org)


;; Provide the package
(provide 'demacs-org)

;;; demacs-org.el ends here
