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


(print *is-windows*)

(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init)  
;;; init.el ends here
