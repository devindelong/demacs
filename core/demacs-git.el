;;; demacs-git.el --- Git configuration.
;;
;;; Commentary:
;;
;; This file contains git configuration.
;;
;; Packages:
;;
;; - magit
;; - git-commit
;; - forge
;; - diff-hl
;;
;;; Code:


;; Magical git.
(use-package magit
  :straight t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))


(use-package git-commit
  :straight t
  :config
  (global-git-commit-mode 1))


;; Integration with Git-hub.
(use-package forge
  :straight t
  :after magit)


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
