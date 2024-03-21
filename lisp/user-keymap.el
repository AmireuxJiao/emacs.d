;;; user-keymap.el --- configuration Emacs keymap -*- lexical-binding: t -*-
;;; Commentary:


;;; Code:

(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

(use-package emacs
  :config (defalias 'yes-or-no-p 'y-or-n-p))
  
(defun user-keymap-hello-world ()
  (interactive)
  (message "user-keymap hello world"))

(provide 'user-keymap)
;;; user-keymap.el ends here


