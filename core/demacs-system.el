;;; demacs-system.el --- System configuration.
;;
;;; Commentary:
;;
;; This file contains system configuration options.
;;
;;
;;; Packages:
;;
;; - persistent-soft
;; - unicode-fonts
;; - ansi-color
;; - all-the-icons
;; - marginalia
;; - all-the-icons-completion
;; - pretty-mode
;; - solaire-mode
;; - diminish
;; - cmake-ide
;; - ripgrep
;; - which-key
;; - exec-path-from-shell
;; - whitespace-cleanup-mode
;; - yasnippet
;; - projectile
;; - helpful
;; - dired
;; - dired-single
;; - all-the-icons-dired
;; - dired-hacks-utils
;; - dired-filter
;; - flycheck
;; - flycheck-posframe
;; - prescient
;; - company
;; - company-prescient
;; - company-posframe
;; - vertico
;; - orderless
;; - emacs
;; - consult
;; - consult-flycheck
;; - consult-company
;; - consult-projectile
;; - consult-lsp
;; - consult-yasnippet
;; - embark
;; - embark-consult
;; - wgrep
;;
;;
;;; Code:


;; -----------------------------------------------------------------------------
;; Tools
;; -----------------------------------------------------------------------------

;; Doxygen formatting.
;; (use-package doxygen-mode
;;   :straight t
;;   :hook ((c++-mode c-mode) . doxygen-mode))

;; Useful for editing grep results:
;;
;; 1) "C-c f" invoke `consult-ripgrep'
;; 2) "C-s-e" invoke `embark-export' (On OS X map that's Ctrl+Cmd+e)
;; 3) "e" or "C-c C-p" invoke `wgrep-change-to-wgrep-mode'
;; 4) Save or cancel
;;    a) Save: "C-x C-s" invoke `save-buffer' (or "C-c C-c")
;;    b) Cancel: "C-c C-k"
;;
(use-package wgrep
  :after (embark-consult ripgrep)
  :straight t)


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


;; Better help.
(use-package helpful
  :straight t
  :bind
  (("C-h k"   . helpful-key)
   ("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-c C-d" . helpful-at-point)
   ("C-h F"   . helpful-function)
   ("C-h C"   . helpful-command)))


(use-package ripgrep
  :straight t)


(use-package which-key
  :straight t
  :custom
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (set-face-attribute
    'which-key-local-map-description-face nil :weight 'bold))


;; A GNU Emacs library to ensure environment variables inside Emacs look the same as in
;; the user's shell. Basically commands on a shell path are visible in emacs
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))


;; Run Docker commands in Emacs.
(use-package docker
  :straight t
  :bind ("C-c d" . docker))

;; -----------------------------------------------------------------------------
;; Miscellaneous
;; -----------------------------------------------------------------------------


;; Persist history over Emacs restarts.
(use-package savehist
  :straight t
  :init
  (savehist-mode))


;; Project management.
(use-package projectile
  :straight t)


(use-package color
  :straight t)


(use-package ansi-color
  :straight t
  :config
  (ansi-color-for-comint-mode-on)
  (setq ansi-color-for-compilation-mode t)
  :hook
  (compilation-filter . ansi-color-compilation-filter))


(use-package persistent-soft
  :straight t)


;; Renders unicode in the buffer.
(use-package pretty-mode
  :straight t
  :hook (text-mode . turn-on-pretty-mode))


;; When we diminish a mode, we are saying we want it to continue doing its work for us,
;; but we no longer want to be reminded of it
(use-package diminish
  :straight t
  :config
  (diminish 'visual-line-mode))



;; prescient.el is a library which sorts and filters lists of candidates, such as appear
;; when you use a package like Ivy or Company.
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode 1))


;; -----------------------------------------------------------------------------
;; Eglot
;; -----------------------------------------------------------------------------

(use-package eglot
  :ensure t
  :hook ((prog-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (global-eglot-inlay-hints-mode nil)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref nil)
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :inlayHintProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider
     :codeActionProvider
     ))
  (eglot-stay-out-of '(yasnippet)))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((c-mode c++-mode objc-mode) . ("/opt/homebrew/opt/llvm/bin/clangd"))))

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------


(use-package unicode-fonts
  :straight t
  :after persistent-soft
  :config
  (unicode-fonts-setup))


;; -----------------------------------------------------------------------------
;; Icons
;; -----------------------------------------------------------------------------


(use-package nerd-icons
  :straight t
  ;; :custom The Nerd Font you want to use in GUI "Symbols Nerd Font Mono" is the default
  ;; and is recommended but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


;; Probably not needed?
(use-package all-the-icons
  :straight t)


;; -----------------------------------------------------------------------------
;; Marginalia
;; -----------------------------------------------------------------------------

;; Adds marginalia to the minibuffer completions. Marginalia are marks or annotations
;; placed at the margin of the page of a book or in this case helpful colorful annotations
;; placed at the margin of the minibuffer for your completion candidates.
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))


;; Nerd icons for marginalia.
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------


;; File browser.
(use-package dired
  :straight (:type built-in)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode)))


;; Adds collapsible sub-menus.
(use-package dired-subtree
        :straight t
        :after dired
        :bind (:map dired-mode-map
                    ("i" . dired-subtree-insert)
                    (";" . dired-subtree-remove)
                    ("<tab>" . dired-subtree-toggle)
                    ("<backtab>" . dired-subtree-cycle)))


;; Collapses nested folders with single elements.
(use-package dired-collapse
  :straight t
  :after dired
  :config (dired-collapse-mode))


(use-package dired-filter
  :straight t
  :after dired)


;; Narrow down the files with search.
(use-package dired-narrow
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("C-s" . dired-narrow)))


(use-package dired-ranger
  :straight t
  :after dired)


(use-package dired-hacks-utils
  :straight t
  :after dired)


(use-package nerd-icons-dired
  :straight t
  :after (dired nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; -----------------------------------------------------------------------------
;; Flycheck
;; -----------------------------------------------------------------------------


;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs, intended as
;; replacement for the older Flymake extension which is part of GNU Emacs.
(use-package flycheck
  :straight t
  :hook (after-init . global-flycheck-mode))


(use-package flycheck-posframe
  :straight t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))


(use-package flycheck-eglot
  :straight t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


;; -----------------------------------------------------------------------------
;; Company
;; -----------------------------------------------------------------------------


;; Completion. Gives us the standard dropdown as-you-type of modern IDEs.
(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode))


(use-package company-prescient
  :straight t
  :after (company prescient)
  :config
  (company-prescient-mode 1))


(use-package company-posframe
  :straight t
  :after company
  :custom
  (company-posframe-quickhelp-delay nil)
  :config
  (company-posframe-mode 1))


;; Python completion.
(use-package company-jedi
  :straight t
  :after company)
(add-to-list 'company-backends 'company-jedi)


;; Adding the dabbrev-code backend becasue company was not auto-completing code
;; from the current working project or source directory.
(add-to-list 'company-backends '(company-dabbrev-code))


;; -----------------------------------------------------------------------------
;; Vertico
;; -----------------------------------------------------------------------------


;; Vertical completions.
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :general
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat
            )
  (:keymaps 'vertico-map
            "<tab>"    #'vertico-insert
            "<escape>" #'minibuffer-keyboard-quit
            "?"        #'minibuffer-completion-help
            "C-M-n"    #'vertico-next-group
            "C-M-p"    #'vertico-previous-group))


;; -----------------------------------------------------------------------------
;; Orderless
;; -----------------------------------------------------------------------------


(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  )


;; -----------------------------------------------------------------------------
;; Emacs
;; -----------------------------------------------------------------------------


(use-package emacs
  :straight t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
   (setq read-extended-command-predicate
         #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; -----------------------------------------------------------------------------
;; Consult
;; -----------------------------------------------------------------------------


(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"   . consult-register-load)
         ("M-'"   . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)             ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s m"   . consult-multi-occur)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("C-s"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)


(use-package consult-flycheck
  :straight t
  :after (consult flyckeck))


(use-package consult-company
  :straight t
  :after (consult company))


(use-package consult-projectile
  :straight t
  :after (consult projectile))


(use-package consult-yasnippet
  :straight t
  :after (consult yasnippet))


;; -----------------------------------------------------------------------------
;; Embark
;; -----------------------------------------------------------------------------


(use-package embark
  :straight t

  :bind
  (("C-."   . embark-act)       ;; pick some comfortable binding
   ("C-;"   . embark-dwim)      ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'demacs-system)
;;; demacs-system.el ends here.
