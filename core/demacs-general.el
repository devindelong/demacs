;;; demacs-general.el --- General configuration.
;;
;;; Commentary:
;;
;; This file contains general configuration options.
;;
;;
;;; Code:


(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; accept 'y' or 'n' instead of yes/no
 ;; the documentation advises against setting this variable
 ;; the documentation can get bent imo
 use-short-answers t
 ;; my source directory
 ;; default-directory "~/src/"
 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 ;; unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; I want to close these fast, so switch to it so I can just hit 'q'
 help-window-select t
 ;; this certainly can't hurt anything
 delete-by-moving-to-trash t
 ;; keep the point in the same place while scrolling
 scroll-preserve-screen-position t
 ;; more info in completions
 completions-detailed t
 ;; highlight error messages more aggressively
 next-error-message-highlight t
 ;; don't let the minibuffer muck up my window tiling
 read-minibuffer-restore-windows t
 ;; scope save prompts to individual projects
 save-some-buffers-default-predicate 'save-some-buffers-root
 ;; don't keep duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; comment empty lines.
 comment-empty-lines t
 )

 ;; Default to 80 for fill column
(setq-default fill-column 80)

;; Prevent backup files from littering the filesystem.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Saves Emacs command history.
(savehist-mode)

;; Highlight matching braces
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; If you enable Delete Selection mode, a minor mode, then inserting text while
;; the mark is active causes the selected text to be deleted first. This is
;; normally default behaviour for text editors.
(delete-selection-mode t)

;; It’s good that Emacs supports the wide variety of file encodings it does, but UTF-8
;; should always, always be the default.
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; Configure UI elements when using the GUI version of Emacs.
(if (window-system)
  (progn
    (tool-bar-mode -1)    ;; Emacs has the ugliest toolbar icons.
    (scroll-bar-mode -1)  ;; Scroll bars don't look nice with the sidebar.
    (tooltip-mode -1)     ;; Annoying.
    (menu-bar-mode 1))    ;; We want the menu bar with MacOS at least.
  (menu-bar-mode -1))

;; Smooth scrolling
(setq scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Forces Emacs to truncate lines that are too long rather than wrapping them
;; Consider doing this only for window-system?
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
(setq-default truncate-lines t)

;; Set default mode
(setq-default major-mode 'text-mode)

(use-package general
  :straight t)

(add-hook 'text-mode-hook 'flyspell-mode)

;; Open Emacs on fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'demacs-general)

;;; demacs-general.el ends here.
