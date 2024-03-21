;;; init.el --- load the full configuration -*- lexical-binding: t -*-

;;; Commentary:


;; This file bootstraps the configuration, which is divided into
;; a number of other files


;;; Code:
(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;;; require module

(require 'init-elpa)
(require 'init-const)
(require 'init-utils)
(require 'user-keymap)
(require 'load-plugins)
(require 'init-ui)

<<<<<<< HEAD
;; 测试中文属性
=======
>>>>>>> c1606d9fa660948f7c26004f5a88c02168781275
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init)  
;;; init.el ends here


