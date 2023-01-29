;;;_demacs-org.el --- Org configuration

;;; Commentary:
;; In this section, Org mode and related packages are configured

;;; Code:

;; Org mode
(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Org mouse for checkboxes
  (require 'org-mouse)
  (setq org-agenda-files (list "~/org/agenda.org"))
  (setq org-support-shift-select t)
  ;; Hide markers for italic, bold etc
  (setq org-hide-emphasis-markers t)
  ;; Print timestamp when done
  (setq org-log-done 'time)
  ;; Handle source blocks
  (setq org-src-tab-acts-natively t)
  ;; Make LaTex use minted when exporting
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex --shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-hide-leading-stars t)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook
  (org-mode . toggle-word-wrap)
  (org-mode . toggle-truncate-lines))

;; Org bullets
(use-package org-bullets
  :straight t
  :defer t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; Org bullets
(use-package toc-org
  :straight t
  :hook
  (org-mode . toc-org-mode))

(use-package org-pomodoro
  :straight t)

(provide 'demacs-org)
;;; demacs-org.el ends here
