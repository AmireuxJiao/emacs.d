;;; load-plugins.el --- Load plugins -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(setq use-package-always-defer t
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(use-package restart-emacs
  :ensure t)

(use-package dashboard
  :ensure t
  :init (dashboard-open)
  :config
  (setq dashboard-banner-logo-title "Wolcome to Emacs Luna!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (projects . 10))))

(use-package gruvbox-theme
  :ensure t
  :init (load-theme 'gruvbox-dark-medium t))

(use-package zenburn-theme
  :ensure t)
  ;;:init (load-theme 'zenburn t))

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
  :bind (("C-c k" . crux-smart-kill-line)
	 ("C-c ^" . crux-top-join-line)))

(use-package amx
  :ensure t
  :init (amx-mode))

;; move cursor
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))

;; 删除前、后的第一个非空字符
(use-package hungry-delete
  :ensure t
  :bind
  (("C-c DEL" . hungry-delete-backward))
  (("C-c d" . hungry-delete-forward)))

(use-package drag-stuff
  :ensure t
  :bind (("M-C-p" . drag-stuff-up)
	 ("M-C-n" . drag-stuff-down)))

(use-package ivy
  :ensure t
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffer t
	search-default-mode #'char-fold-to-regexp
	ivy-count-format "(%d/%d) "
	ivy-initial-inputs-alist nil)
  :bind
  (("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   ;; :when ((*is-mac*)
   ;; 	  ("C-x C-@" . 'counsel-mark-ring))
   ))

(use-package ivy-posframe
  :ensure t
  :delight
  :init
  (setq ivy-posframe-display-functions-alist
	'((swiper . ivy-posframe-display-at-frame-center)
	  (complete-symbol . ivy-posframe-display-at-point)
	  (counsel-M-x . ivy-posframe-display-at-frame-center)
	  (counsel-find-file . ivy-posframe-display-at-frame-center)
	  (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
	  (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))
  

(use-package embark
  :ensure t
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config (add-to-list 'display-buffer-alist
		       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
			 (window-parameters (mode-line-format . none))))
  :bind (("C-." . embark-bact)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings)))

;; TODO configuration avy plugins
(use-package avy
  :ensure t
  :config
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
		(alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
		(alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?u ?j ?l))
  :bind (("M-j" . avy-goto-char-timer)
	 ("C-j a g" . avy-goto-line)))

(use-package counsel
  :ensure t
  :after (ivy)
  :bind (("M-x" . 'counsel-M-x)
	 ("C-x C-f" . 'counsel-find-file)
	 ("C-c f" . 'counsel-recentf)
	 ("C-c g" . 'counsel-git)))

(use-package swiper
  :ensure t
  :after (ivy)
  :bind (("C-s" . 'swiper)
	 ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t))


(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-tooltip-align-annotations t
	company-tooltip-idle-delay 0.0
	company-show-numbers t
	company-selection-wrap-around t
	company-transformers '(company-sort-by-occurrence)))

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)
					       (member 'company-yasnippet backend)))
	backend (append (if (consp backend) backend (list backend))
			'(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :custom (undo-tree-auto-save-history nil))

(use-package flycheck
  :ensure t
  :config (setq flycheck-emacs-lisp-load-path 'inherit)
  :hook (after-init . global-flycheck-mode))
;; :hook (prog-mode . flycheck-mode) ;; 只在编程模式中使用

(use-package ace-window
  :ensure t
  :bind ("C-x o" . 'ace-window))

(use-package beacon
  :ensure f
  ;;:init (if (*is_win))(beacon-mode 1)
  :config (setq beacon-size 70
		beacon-color "#9BCD9B"
		beacon-blink-delay 0.5))

(use-package cmake-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (python-mode . lsp-deferred)
	 (cmake-ts-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (javascript-mode . lsp-deferred))
  :commands (lsp lsp-deferred))




(defun plugins-hello-world ()
  "Check load-plugins.el is useful."
  (interactive)
  (message "plugins hello world"))

(provide 'load-plugins)
;;; load-plugins.el ends here
