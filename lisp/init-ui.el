;;; init-ui.el --- Set up basic ui and functionality for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (set-frame-font "Mononoki Nerd Font Mono 10" nil t)
;; 设置文件编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq dafault-buffer-file-coding-system 'utf-8)
;; 设置光标
(setq-default cursor-type '(bar . 3))
(pixel-scroll-precision-mode t)
;; 设置垃圾回收阈值
(setq gc-cons-threshold most-positive-fixnum)
;; 基础设置
(ido-mode 1)
(defconst *spell-check-support-enable* t)
(setq confirm-kill-emacs #'yes-or-no-p)      ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t)                       ; 自动补全括号
(add-hook 'prog-mode-hook #'hs-minor-mode)   ; 编程模式下，可以折叠代码块
(add-hook 'prog-mode-hook #'show-paren-mode) ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t)                       ; 在 Mode line 上显示列号
(global-auto-revert-mode t)                  ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                    ; 选中文本后输入文本会替换文本（更符合我们习惯了的其它编辑器的逻辑）
;; (setq inhibit-startup-message t)             ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                 ; 关闭文件自动备份

(tool-bar-mode -1)                           ; （熟练后可选）关闭 Tool bar
(when (display-graphic-p) (toggle-scroll-bar -1)) ; 图形界面时关闭滚动条

(global-display-line-numbers-mode 1)         ; 在 Window 显示行号
(setq display-line-numbers-type 'relative)
  
;; 配置Windows下的中英文显示问题.
(use-package emacs
  :if (display-graphic-p)
  :config
  ;; Font setting
  (if *is-windows*
      (progn
	(set-face-attribute 'default nil :font "Mononoki Nerd Font Mono 10") ;; 代码字体
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset (font-spec :family "Maple Mono SC NF" :size 12))))) ;; 中文字体
  (if *is-linux*
      (progn
	(set-face-attribute 'default nil :font "Mononoki Nerd Font Mono 10") ;; 代码字体
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset (font-spec :family "Maple Mono SC NF" :size 14))))) ;; 中文字体
  (if (not (eq window-system nil))
      (progn
	;; top, left ... must be integer
	(add-to-list 'default-frame-alist
		     (cons 'top (/ (x-display-pixel-height) 10)))
	(add-to-list 'default-frame-alist
		     (cons 'left (/ (x-display-pixel-width) 10)))
	(add-to-list 'default-frame-alist
		     (cons 'height (/ (* 4 (x-display-pixel-height))
				      (* 5 (frame-char-height)))))
	(add-to-list 'default-frame-alist
		     (cons 'width (/ (* 4 (x-display-pixel-height))
				      (* 5 (frame-char-width)))))))

  (global-display-line-numbers-mode 1)         ; 在 Window 显示行号
  (setq display-line-numbers-type 'relative)

  
  )

(provide 'init-ui)
;;; init-ui.el ends here
