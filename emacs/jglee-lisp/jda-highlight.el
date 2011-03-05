;;; j-highlight.el --- highlight symbol
;;
;; Copyright (C) 2010 Lee Jong-Gyu<jglee1027@gmail.com>
;;
;; Authors: Lee Jong-Gyu<jglee1027@gmail.com>
;; Version: 0.1.0
;; Repository: git://github.com/jglee1027/settings.git
;; 
;; This file is NOT part of GNU Emacs.
;;
;; * License
;; 	 This program is free software; you can redistribute it and/or modify
;; 	 it under the terms of the GNU General Public License as published by
;; 	 the Free Software Foundation; either version 2, or (at your option)
;; 	 any later version.
;; 
;; 	 This program is distributed in the hope that it will be useful,
;; 	 but WITHOUT ANY WARRANTY; without even the implied warranty of
;; 	 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; 	 GNU General Public License for more details.
;; 
;; 	 You should have received a copy of the GNU General Public License
;; 	 along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;; 
;; * Installation
;;   Edit your ~/.emacs file to add the line:
;;     (add-to-list 'load-path "/path/to/j-highlight.el")
;;     (require 'j-highlight)
;; 
;; * Major commands:
;;

;;; Code:
(require 'hi-lock)
(require 'thingatpt)

;; ======================================================================
;; highlight the symbol at current point with a idle timer
;; ======================================================================
(defvar j-highlight-symbol-color 'isearch)
(defvar j-highlight-symbol-timer-interval 0.7)

(defvar j-highlight-symbol-timer nil)
(defvar j-highlight-symbol-buffers-alist nil)
(defvar j-highlight-symbol-symbol-regex nil)
(make-variable-buffer-local 'j-highlight-symbol-symbol-regex)

;; callback function 
(defun j-highlight-symbol-callback ()
  (save-excursion
	(let* ((symbol (symbol-at-point))
		   (symbol-regex (cond ((and symbol)
								(concat "\\<"
										(regexp-quote (symbol-name symbol))
										"\\>"))
							   (t
								nil))))
	  (cond ((and symbol-regex
				  (not (string= symbol-regex j-highlight-symbol-symbol-regex))
				  (buffer-file-name))
			 (hi-lock-unface-buffer j-highlight-symbol-symbol-regex)
			 (hi-lock-face-buffer symbol-regex
								  j-highlight-symbol-color)
			 (setq j-highlight-symbol-buffers-alist
				   (assq-delete-all (current-buffer)
									j-highlight-symbol-buffers-alist))
			 (add-to-list 'j-highlight-symbol-buffers-alist
						  (list (current-buffer) symbol-regex) t)
			 (setq j-highlight-symbol-symbol-regex symbol-regex))))))

;; start function
(defun j-highlight-symbol-run-toggle ()
  (interactive)
  (cond ((timerp j-highlight-symbol-timer)
		 (cancel-timer j-highlight-symbol-timer)
		 (while j-highlight-symbol-buffers-alist
		   (let* ((entry (pop j-highlight-symbol-buffers-alist))
				  (buffer (pop entry))
				  (symbol-regex (pop entry)))
			 (condition-case nil
				 (progn
				   (set-buffer buffer)
				   (hi-lock-unface-buffer j-highlight-symbol-symbol-regex))
			   (error nil))))
		 (message "j-highlight-symbol off.")
		 (setq j-highlight-symbol-timer nil))
		(t
		 (unless hi-lock-mode (hi-lock-mode 1))
		 (setq j-highlight-symbol-timer
			   (run-with-idle-timer j-highlight-symbol-timer-interval t 'j-highlight-symbol-callback))
		 (message "j-highlight-symbol on."))))

(define-key global-map (kbd "C-c j h") 'j-highlight-symbol-run-toggle)

;;; j-highligt.el ends here
