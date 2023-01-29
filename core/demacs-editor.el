;;; demacs-editor.el --- Editor configuration.

;;; Commentary:
;; This file contains editor configuration options.

;; The following packages are loaded:
;; - multiple-cursors
;; - whitespace-cleanup-mode
;; - highlight-numbers

;;; Code:


;; Default tab width.
(defvar demacs/c-mode-tab-width 2
  "Default tab width for C, C++, Java, Objective-C.")


;; If you enable Delete Selection mode, a minor mode, then inserting text while
;; the mark is active causes the selected text to be deleted first.
;; This is normally default behaviour for text editors.
(delete-selection-mode t)

;; ----------------------------------------------------------------------------
;;
;; Functions
;;
;; ----------------------------------------------------------------------------

(defun demacs/untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))


(defun demacs/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))


(defun demacs/comment-or-uncomment-region-or-line ()
  "Toggle comments for the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))


(defun demacs/toggle-comment()
  "Toggle commenting/uncommenting a region or line."
  (demacs/comment-or-uncomment-region-or-line))

;; ----------------------------------------------------------------------------
;;
;; Programming mode hooks
;;
;; ----------------------------------------------------------------------------

(defun demacs/prog-mode-hook ()
  "Customization for all programming modes."
  (setq
   ;; Default to inserting spaces instead of tabs.
   indent-tabs-mode nil
   ;; If the value of the variable require-final-newline is t, saving or writing a file
   ;; silently puts a newline at the end if there isn't already one there.
   require-final-newline t))


(defun demacs/c-mode-common-hook ()
  "Customizations for of 'c-mode', 'c++-mode', 'objc-mode', 'java-mode'."
  (setq
   ;; The "style"
   c-default-style "ellemtel"
   ;; Something.
   c-basic-offset demacs/c-mode-tab-width
   ;; Custom tab width.
   tab-width demacs/c-mode-tab-width
   ;; Ensure that tabs are not inderted.
   indent-tabs-mode nil)
  ;; Ensure that substatement indentation brackets do not get indented.
  (c-set-offset 'substatement-open 0))


(defun demacs/makefile-mode-hook ()
  "Customization for makegile mode."
  (setq indent-tabs-mode t))


;; ----------------------------------------------------------------------------
;;
;; Packages
;;
;; ----------------------------------------------------------------------------

;; Whitespace cleanup
(use-package whitespace-cleanup-mode
  :straight t
  :custom
  (show-trailing-whitespace t)
  :config
  (global-whitespace-cleanup-mode 1))

;; YASnippet is a template system for Emacs. It allows you to type an
;; abbreviation and automatically expand it into function templates.
;; Bundled language templates include: C, C++, C#, Perl, Python, Ruby,
;; SQL, LaTeX, HTML, CSS and more.
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  :custom
  (yas-prompt-functions '(yas-completing-prompt)))


;; ----------------------------------------------------------------------------
;;
;; Hooks
;;
;; ----------------------------------------------------------------------------

(add-hook 'prog-mode-hook #'demacs/prog-mode-hook)
(add-hook 'c-mode-common-hook #'demacs/c-mode-common-hook)
(add-hook 'makefile-mode-hook #'demacs/makefile-mode-hook)

;; Convert tabs to spaces upon saving the file.
(add-hook 'before-save-hook #'demacs/untabify-except-makefiles)

;; Delete trailing whitespace upon saving the file.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Turn on electric pair mode for now.
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Only add line numbers when editing code.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Use visual line mode when compiling.
(add-hook 'compilation-mode-hook 'visual-line-mode)


;; -----------------------------------------------------------------------------
;;
;; Key Bindings
;;
;; -----------------------------------------------------------------------------

;; Will have to figure this out.
(bind-key "C-/"   #'demacs/comment-or-uncomment-region-or-line)
(bind-key "C-c q" #'fill-paragraph)
(bind-key "C-c Q" #'set-fill-column)
(bind-key "C-c I" #'demacs/indent-just-yanked)

;; When split, if a buffer is killed, also its window is killed
(bind-key "C-x k" #'kill-buffer)
(bind-key "C-j"   #'newline-and-indent)

(provide 'demacs-editor)

;;; demacs-editor.el ends here.
