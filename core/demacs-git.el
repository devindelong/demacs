;;; demacs-git.el --- Git configuration.

;;; Commentary:
;; This file contains git configuration options.

;; The following packages are loaded:
;; - multiple-cursors
;; - whitespace-cleanup-mode
;; - highlight-numbers

;;; Code:


;;
(use-package magit
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (defun demacs/commit-hook () (set-fill-column 80))
  (add-hook 'git-commit-setup-hook #'demacs/commit-hook)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;;
;; (use-package libgit
;;   :straight t
;;   :after magit)

;;
;; (use-package magit-libgit
;;   :straight t
;;   :after (magit libgit))

;;
(use-package git-commit
  :straight t
  :config
  (global-git-commit-mode 1))

;; Integration with Git-hub.
;; Also integrates with other "forges"?
(use-package forge
  :straight t
  :after magit)

;; hack to eliminate weirdness
(unless (boundp 'bug-reference-auto-setup-functions)
  (defvar bug-reference-auto-setup-functions '()))

;; Highlights the git gutter.
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-margin-symbols-alist
   '((insert . " ")
     (delete . " ")
     (change . " ")
     (unknown . "?")
     (ignored . "i"))))

(provide 'demacs-git)

;;; demacs-git.el ends here.
