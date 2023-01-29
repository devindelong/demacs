;;; demacs-ui.el --- UI configuration.

;;; Commentary:
;; This file contains UI configuration options.

;; The following packages are loaded:
;; - bar-cursor
;; - highlight-indent-guides
;; - hl-line


;;; Code:

;; -----------------------------------------------------------------------------
;;
;; Window Managenent
;;
;; -----------------------------------------------------------------------------

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

;; (defun demacs/restore-saved-window-size()
;;   (unless (load "~/.emacs.d/whsettings" t nil t)
;;     (setq saved-window-size '(80 30)))
;;   (nconc default-frame-alist `((width . ,(car saved-window-size))
;;                    (height . ,(cadr saved-window-size)))))


;; (defun demacs/save-window-size-if-changed (&optional unused)
;;   (let ((original-window-size  `(,(frame-width) ,(frame-height))))
;;     (unless (equal original-window-size saved-window-size)
;;       (with-temp-buffer
;;         (setq saved-window-size original-window-size)
;;         (insert (concat "(setq saved-window-size '"
;;                         (prin1-to-string saved-window-size) ")"))
;;         (write-file "~/.emacs.d/whsettings")))))

;;  ;; Restore the saved window size.
;; (add-hook 'after-init-hook #'demacs/restore-saved-window-size)

;; ;; Save the window size if it changes,
;; (add-hook 'window-size-change-functions #'demacs/save-window-size-if-changed)

;; Key bindings.
(bind-key "C-x 0" #'demacs/close-window)
(bind-key "C-x 2" #'demacs/split-window-horizontally)
(bind-key "C-x 3" #'demacs/split-window-vertically)


;; -----------------------------------------------------------------------------
;;
;; Cursor/Line/Indentation
;;
;; -----------------------------------------------------------------------------

;; Bar Cursor Mode
;; Emacs Lisp package that changes the Emacs cursor from a block into a bar. In
;; overwrite-mode, the cursor will change into a block.
(use-package bar-cursor
  :straight t
  :after diminish
  :diminish
  :config
  (bar-cursor-mode 1))

;; Support for multiple cursor selection and editing.
(use-package multiple-cursors
  :straight t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-SPC" . mc/mark-all-like-this)
         ))

;; Custom syntax highlighting
;; Highlight numbers mode
(use-package highlight-numbers
  :straight t
  :diminish
  :config
  (set-face-attribute 'highlight-numbers-number nil :weight 'normal)
  :hook
  (prog-mode . highlight-numbers-mode))

;; Highlight the current line.
(use-package hl-line
  :straight t
  :hook
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))

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

;; -----------------------------------------------------------------------------
;;
;; Doom modeline
;;
;; -----------------------------------------------------------------------------

(use-package doom-modeline
  :straight t
  :config
  (column-number-mode)
  (size-indication-mode)
  (setq doom-modeline-buffer-file-name-style 'auto
        doom-modeline-height 30
        doom-modeline-major-mode-color-icon nil
        doom-modeline-lsp t
        doom-modeline-env-version t)
  :init
  (doom-modeline-mode)
  :hook
  (after-init . doom-modeline-mode))


;; -----------------------------------------------------------------------------
;;
;; Treemacs
;;
;; -----------------------------------------------------------------------------

(use-package treemacs
  :straight t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          ;; treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          ;; treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           t
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
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)

(add-hook 'emacs-startup-hook 'treemacs)


;; -----------------------------------------------------------------------------
;;
;; Centausr Tabs
;;
;; -----------------------------------------------------------------------------

(use-package centaur-tabs
  :straight t
  :config
  (centaur-tabs-mode t)

  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-enable-ido-completion nil)
  (centaur-tabs-style "bar")
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-height 28)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-modified-marker "")
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)

  ;; :hook
  ;; (dashboard-mode . centaur-tabs-local-mode)
  ;; (term-mode . centaur-tabs-local-mode)
  ;; (calendar-mode . centaur-tabs-local-mode)
  ;; (org-agenda-mode . centaur-tabs-local-mode)
  ;; (helpful-mode . centaur-tabs-local-mode)

  :init
  (setq centaur-tabs-enable-key-bindings t)

  :bind
  (("C-{" . #'centaur-tabs-backward)
   ("C-}" . #'centaur-tabs-forward)
   ("C-|" . #'centaur-tabs-toggle-groups)))


;; -----------------------------------------------------------------------------
;;
;; Themes
;;
;; -----------------------------------------------------------------------------

(use-package kaolin-themes
  :straight t)

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (defvar doom-themes-treemacs-theme "doom-colors")

  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun demacs/init-theme ()
  (load-theme 'doom-opera t))

(add-hook 'emacs-startup-hook #'demacs/init-theme)

(provide 'demacs-ui)

;;; demacs-ui.el ends here.
