;;; load-plugins.el --- Load plugins -*- lexical-binding: t -*-
;;; Comments:

;;; Code:

(setq use-package-always-defer t
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(use-package restart-emacs
  :ensure t)

(use-package gruvbox-theme
  :ensure t
  :init (load-theme 'gruvbox-dark-soft t))

(use-package smart-mode-line
  :ensure t
  :init (setq sml/no-confirm-load-theme t
	      sml/theme 'respectful)
  (sml/setup))

;; 启动时间
(use-package benchmark-init
  :ensure t
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; 删除当前行
(use-package crux
  :ensure t
  :bind ("C-c k" . crux-smart-kill-line))

;; 删除前、后的第一个非空字符
(use-package hungry-delete
  :ensure t
  :bind
  (("C-c DEL" . hungry-delete-backward))
  (("C-c d" . hungry-delete-forward)))

(use-package dashboard
  :ensure t)

(use-package drag-stuff
  :ensure t
  :bind (("M-C-p" . drag-stuff-up)
	 ("M-C-n" . drag-stuff-down)))

(defun plugins-hello-world ()
  (interactive)
  (message "plugins hello world"))

(provide 'load-plugins)
;;; load-plugins.el ends here
  
