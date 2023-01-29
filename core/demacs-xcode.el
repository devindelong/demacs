;;; demacs-xcode.el --- XCode commands.

;;; Commentary:
;; This file contains various helper commands for XCode.

;;; Code:


(defun demacs/xcode-open-current-file()
  (interactive)
  (shell-command-to-string
    (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name))))

(global-set-key (kbd "C-c p o") #'demacs/xcode-open-current-file)

(provide demacs-xcode)
;;; demacs-xcode.el ends here.
