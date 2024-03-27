;;; init-const.el --- This file defines constants -*- lexical-binding: t —*-
;;; Commentart
n
;;; Code
(defconst *is-mac* (eq system-type 'darwin)
  "判断是否是mac")

(defconst *is-linux* (eq system-type 'gnu/linux)
  "判断是否是Linux")

(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  "判断是否是Windows")

(provide 'init-const)
;;; init-const.el ends here



