;;; demacs-ui.el --- UI configuration.
;;
;;; Commentary:
;;
;; This file contains UI configuration options.
;;
;;; Packages:
;;
;; - bar-cursor
;; - multiple-cursors
;; - highlight-numbers
;; - hl-line
;; - highlight-indent-guides
;; - doom-modeline
;; - treemacs
;; - treemacs-projectile
;; - treemacs-magit
;; - centaur-tabs
;; - kaolin-themes
;; - doom-themes
;;
;;; Code:

;;
;; Window Managenent
;;

(defun demacs/split-window-horizontally ()
  "Split a window horizontally and balance them."
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))

(defun demacs/split-window-vertically ()
  "Split a window vertically and balance them."
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))

(defun demacs/close-window ()
  (interactive)
  (delete-window)
  (balance-windows))

;; Key bindings.
(bind-key "C-x 0" #'demacs/close-window)
(bind-key "C-x 2" #'demacs/split-window-horizontally)
(bind-key "C-x 3" #'demacs/split-window-vertically)

;;
;; Bar Cursor
;;
;; Emacs Lisp package that changes the Emacs cursor from a block into a bar. In
;; overwrite-mode, the cursor will change into a block.
;;

(use-package bar-cursor
  :straight t
  :after diminish
  :diminish
  :config
  (bar-cursor-mode 1))

;;
;; Multiple cursors
;;
;; Support for multiple cursor selection and editing.
;;

(use-package multiple-cursors
  :straight t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-SPC" . mc/mark-all-like-this)))

;;
;; Highlight numbers
;;
;; Custom syntax highlighting for numbers.
;;

(use-package highlight-numbers
  :straight t
  :diminish
  :config
  (set-face-attribute 'highlight-numbers-number nil :weight 'normal)
  :hook
  (prog-mode . highlight-numbers-mode))

;;
;; HL line
;;
;; Highlight the current line.
;;

(use-package hl-line
  :straight t
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))


;;
;; Highlight indent guides
;;

(use-package highlight-indent-guides
  :straight t
  :config
  (progn
    (setq
     ;; Use bitmaps instead of characters.
     highlight-indent-guides-method 'bitmap
     ;; Responsive guides allow you to visualize not only the indentation
     ;; itself, but your place in it. To enable this feature, customize
     ;; highlight-indent-guides-responsive, and set it to one of the following:
     ;;
     ;;   nil: The default. Responsive guides are disabled.
     ;;
     ;;   top: Use a different color to highlight the "current" guide (the
     ;;        indentation block of the line that the cursor is on). This
     ;;        changes as the cursor moves.
     ;;
     ;;   stack: Like top, but also use a third color for all "ancestor" guides
     ;;          of the current guide. Again, this will change as the cursor
     ;;          moves around.
     highlight-indent-guides-responsive 'stack

     ;; If you're using the 'bitmap display method, you may set a custom bitmap
     ;; function, which determines what your guides will look like. Customize
     ;; highlight-indent-guides-bitmap-function, and set it to:
     ;;
     ;;   highlight-indent-guides--bitmap-dots: A guide is a column of small dots.
     ;;                                         This is the default.
     ;;
     ;;   highlight-indent-guides--bitmap-line: A guide is a solid vertical line.
     ;;
     ;; Or, write your own.
     highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

  :hook
  ;; Only display indentation guides when in a programming mode.
  (prog-mode . highlight-indent-guides-mode))

;;
;; Doom modeline
;;

(use-package doom-modeline
  :straight t
  :config
  (column-number-mode)
  (size-indication-mode)
  (setq doom-modeline-buffer-file-name-style 'auto
        doom-modeline-height 30
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-lsp t
        doom-modeline-env-version t
        )
  :hook
  (after-init . doom-modeline-mode))

;;
;; Treemacs
;;

(use-package treemacs
  :straight t
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          ;; treemacs-indentation-string              "\n "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
        )
  )

;;
;; Treemacs projectile
;;

(use-package treemacs-projectile
  :straight t
  :diminish
  :after (treemacs projectile))

;;
;; Treemacs magit
;;

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)

;;
;; Treemacs dired icons
;;

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)

;;
;; Treemacs Hooks
;;

;; Run treemacs on startup.
(add-hook 'emacs-startup-hook 'treemacs)

;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;
;; Centaur Tabs
;;

(use-package centaur-tabs
  :straight t
  :after (all-the-icons)
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-close-button t
        centaur-tabs-enable-ido-completion nil
        ;; Tab styles do not render properly in newer Emacs
        centaur-tabs-style 'rounded
        centaur-tabs-set-modified-marker t
        centaur-tabs-height 32
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-modified-marker ""
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-group-by-projectile-project t)

  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  :bind
  (
    ("C-{" . #'centaur-tabs-backward)
    ("C-}" . #'centaur-tabs-forward)
    ("C-|" . #'centaur-tabs-toggle-groups)
  )
) ;; centaur-tabs


;; Provide this package.
(provide 'demacs-ui)

;;; demacs-ui.el ends here.
