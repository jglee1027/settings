(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))

;; variable for the timer object
(defvar j-highlight-timer-timer nil)

;; callback function 
(defun j-highlight-timer-callback ()
  (save-excursion
	(isearch-yank-symbol)))

;; start functions
(defun j-highlight-timer-run-toggle ()
  (interactive)
  (if (timerp j-highlight-timer-timer)
	  (progn
		(cancel-timer j-highlight-timer-timer)
		(message "j-highlight-timer off.")
		(setq j-highlight-timer-timer nil))
	(progn
	  (setq j-highlight-timer-timer
			(run-with-idle-timer 1.5 t 'j-highlight-timer-callback))
	  (message "j-highlight-timer on."))))

(define-key global-map (kbd "C-x j h") 'j-highlight-timer-run-toggle)
