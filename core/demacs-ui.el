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
        doom-modeline-major-mode-color-icon nil
        doom-modeline-lsp t
        doom-modeline-env-version t)
  :init
  (doom-modeline-mode)
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

;; Run treemacs on startup.
(add-hook 'emacs-startup-hook 'treemacs)

;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; (use-package centaur-tabs
;;   :straight t

;;   :config
;;   (centaur-tabs-mode t)

;;   :custom
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-show-new-tab-button nil)
;;   (centaur-tabs-set-close-button nil)
;;   (centaur-tabs-enable-ido-completion nil)
;;   (centaur-tabs-style "alternate")
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-height 34)
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-modified-marker "")
;;   (uniquify-separator "/")
;;   (uniquify-buffer-name-style 'forward)

;;   ;; :hook
;;   ;; (dashboard-mode . centaur-tabs-local-mode)
;;   ;; (term-mode . centaur-tabs-local-mode)
;;   ;; (calendar-mode . centaur-tabs-local-mode)
;;   ;; (org-agenda-mode . centaur-tabs-local-mode)
;;   ;; (helpful-mode . centaur-tabs-local-mode)

;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)

;;   :bind
;;   (
;;     ("C-{" . #'centaur-tabs-backward)
;;     ("C-}" . #'centaur-tabs-forward)
;;     ("C-|" . #'centaur-tabs-toggle-groups)
;;   )
;; )



(unless (version< emacs-version "27")
  (use-package tab-line
    :straight t
    :hook (after-init . global-tab-line-mode)
    :config
    (defun tab-line-close-tab (&optional e)
      "Close the selected tab.

If tab is presented in another window, close the tab by using
`bury-buffer` function.  If tab is unique to all existing
windows, kill the buffer with `kill-buffer` function.  Lastly, if
no tabs left in the window, it is deleted with `delete-window`
function."
      (interactive "e")
      (let* ((posnp (event-start e))
             (window (posn-window posnp))
             (buffer (get-pos-property 1 'tab (car (posn-string posnp)))))
        (with-selected-window window
          (let ((tab-list (tab-line-tabs-window-buffers))
                (buffer-list (flatten-list
                              (seq-reduce (lambda (list window)
                                            (select-window window t)
                                            (cons (tab-line-tabs-window-buffers) list))
                                          (window-list) nil))))
            (select-window window)
            (if (> (seq-count (lambda (b) (eq b buffer)) buffer-list) 1)
                (progn
                  (if (eq buffer (current-buffer))
                      (bury-buffer)
                    (set-window-prev-buffers window (assq-delete-all buffer (window-prev-buffers)))
                    (set-window-next-buffers window (delq buffer (window-next-buffers))))
                  (unless (cdr tab-list)
                    (ignore-errors (delete-window window))))
              (and (kill-buffer buffer)
                   (unless (cdr tab-list)
                     (ignore-errors (delete-window window)))))))))

    (defcustom tab-line-tab-min-width 10
      "Minimum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    (defcustom tab-line-tab-max-width 30
      "Maximum width of a tab in characters."
      :type 'integer
      :group 'tab-line)

    (defun aorst/tab-line-name-buffer (buffer &rest _buffers)
      "Create name for tab with padding and truncation.

If buffer name is shorter than `tab-line-tab-max-width' it gets
centered with spaces, otherwise it is truncated, to preserve
equal width for all tabs.  This function also tries to fit as
many tabs in window as possible, so if there are no room for tabs
with maximum width, it calculates new width for each tab and
truncates text if needed.  Minimal width can be set with
`tab-line-tab-min-width' variable."
      (with-current-buffer buffer
        (let* ((window-width (window-width (get-buffer-window)))
               (tab-amount (length (tab-line-tabs-window-buffers)))
               (window-max-tab-width (if (>= (* (+ tab-line-tab-max-width 3) tab-amount) window-width)
                                         (/ window-width tab-amount)
                                       tab-line-tab-max-width))
               (tab-width (- (cond ((> window-max-tab-width tab-line-tab-max-width)
                                    tab-line-tab-max-width)
                                   ((< window-max-tab-width tab-line-tab-min-width)
                                    tab-line-tab-min-width)
                                   (t window-max-tab-width))
                             3)) ;; compensation for ' x ' button
               (buffer-name (string-trim (buffer-name)))
               (name-width (length buffer-name)))
          (if (>= name-width tab-width)
              (concat  " " (truncate-string-to-width buffer-name (- tab-width 2)) "…")
            (let* ((padding (make-string (+ (/ (- tab-width name-width) 2) 1) ?\s))
                   (buffer-name (concat padding buffer-name)))
              (concat buffer-name (make-string (- tab-width (length buffer-name)) ?\s)))))))

    (setq tab-line-close-button-show t
          tab-line-new-button-show nil
          tab-line-separator ""
          tab-line-tab-name-function #'aorst/tab-line-name-buffer
          tab-line-right-button (propertize (if (char-displayable-p ?▶) " ▶ " " > ")
                                            'keymap tab-line-right-map
                                            'mouse-face 'tab-line-highlight
                                            'help-echo "Click to scroll right")
          tab-line-left-button (propertize (if (char-displayable-p ?◀) " ◀ " " < ")
                                           'keymap tab-line-left-map
                                           'mouse-face 'tab-line-highlight
                                           'help-echo "Click to scroll left")
          tab-line-close-button (propertize (if (char-displayable-p ?×) " × " " x ")
                                            'keymap tab-line-tab-close-map
                                            'mouse-face 'tab-line-close-highlight
                                            'help-echo "Click to close tab"))

    (let ((bg (if (facep 'solaire-default-face)
                  (face-attribute 'solaire-default-face :background)
                (face-attribute 'default :background)))
          (fg (face-attribute 'default :foreground))
          (base (face-attribute 'mode-line :background))
          (box-width (/ (line-pixel-height) 2)))
      (set-face-attribute 'tab-line nil :background base :foreground fg :height 1.0 :inherit nil :box (list :line-width -1 :color base))
      (set-face-attribute 'tab-line-tab nil :foreground fg :background bg :weight 'normal :inherit nil :box (list :line-width box-width :color bg))
      (set-face-attribute 'tab-line-tab-inactive nil :foreground fg :background base :weight 'normal :inherit nil :box (list :line-width box-width :color base))
      (set-face-attribute 'tab-line-tab-current nil :foreground fg :background bg :weight 'normal :inherit nil :box (list :line-width box-width :color bg)))

    (dolist (mode '(ediff-mode
                    process-menu-mode
                    term-mode
                    vterm-mode))
      (add-to-list 'tab-line-exclude-modes mode))))


(bind-key "C-}" #'tab-line-switch-to-next-tab)
(bind-key "C-{" #'tab-line-switch-to-prev-tab)

;; Provide this package.
(provide 'demacs-ui)

;;; demacs-ui.el ends here.
