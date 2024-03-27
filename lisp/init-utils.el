;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary;:

;;; Code

;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
	(goto-char pt)
	(embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0)))) t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr (ring-ref avy-ring 0))) t)

;; (defun pulse-line (&rest _)
;;   "pulse the current line."
;;   (pulse-momentary-highlight-one-line (point)))

;; (dolist (command
;; 	 '(scroll-up-command scroll-down-command recenter-top-bottom other-window swiper))
;;   (advice-add command :after #'pulse-line))

(provide 'init-utils)
;;; init-utils.el ends here
