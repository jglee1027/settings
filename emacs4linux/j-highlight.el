(defun isearch-yank-symbol ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if (and sym
			 (not (string= sym j-highlight-symbol-old-symbol)))
        (progn
          (setq isearch-regexp t
                isearch-string (concat "\\_<" (regexp-quote (symbol-name sym)) "\\_>")
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t
				j-highlight-symbol-old-symbol sym))
      (ding)))
  (isearch-search-and-update))

;; variable for the timer object
(defvar j-highlight-symbol-timer nil)
(defvar j-highlight-symbol-old-symbol nil)

;; callback function 
(defun j-highlight-symbol-callback ()
  (save-excursion
	(isearch-yank-symbol)))

;; start functions
(defun j-highlight-symbol-run-toggle ()
  (interactive)
  (if (timerp j-highlight-symbol-timer)
	  (progn
		(cancel-timer j-highlight-symbol-timer)
		(message "j-highlight-symbol off.")
		(setq j-highlight-symbol-timer nil))
	(progn
	  (setq j-highlight-symbol-timer
			(run-with-idle-timer 1.5 t 'j-highlight-symbol-callback))
	  (message "j-highlight-symbol on."))))

(define-key global-map (kbd "C-x j h") 'j-highlight-symbol-run-toggle)
