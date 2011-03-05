;;; jda-highlight.el --- highlight symbol
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
;;     (add-to-list 'load-path "/path/to/jda-highlight.el")
;;     (require 'jda-highlight)
;; 
;; * Major commands:
;;

;;; Code:
(require 'hi-lock)
(require 'thingatpt)

;; ======================================================================
;; highlight the symbol at current point with a idle timer
;; ======================================================================
(defvar jda-highlight-symbol-color 'isearch)
(defvar jda-highlight-symbol-timer-interval 0.7)

(defvar jda-highlight-symbol-timer nil)
(defvar jda-highlight-symbol-buffers-alist nil)
(defvar jda-highlight-symbol-symbol-regex nil)
(make-variable-buffer-local 'jda-highlight-symbol-symbol-regex)

;; callback function 
(defun jda-highlight-symbol-callback ()
  (save-excursion
	(let* ((symbol (symbol-at-point))
		   (symbol-regex (cond ((and symbol)
								(concat "\\<"
										(regexp-quote (symbol-name symbol))
										"\\>"))
							   (t
								nil))))
	  (cond ((and symbol-regex
				  (not (string= symbol-regex jda-highlight-symbol-symbol-regex))
				  (buffer-file-name))
			 (hi-lock-unface-buffer jda-highlight-symbol-symbol-regex)
			 (hi-lock-face-buffer symbol-regex
								  jda-highlight-symbol-color)
			 (setq jda-highlight-symbol-buffers-alist
				   (assq-delete-all (current-buffer)
									jda-highlight-symbol-buffers-alist))
			 (add-to-list 'jda-highlight-symbol-buffers-alist
						  (list (current-buffer) symbol-regex) t)
			 (setq jda-highlight-symbol-symbol-regex symbol-regex))))))

;; start function
(defun jda-highlight-symbol-run-toggle ()
  (interactive)
  (cond ((timerp jda-highlight-symbol-timer)
		 (cancel-timer jda-highlight-symbol-timer)
		 (while jda-highlight-symbol-buffers-alist
		   (let* ((entry (pop jda-highlight-symbol-buffers-alist))
				  (buffer (pop entry))
				  (symbol-regex (pop entry)))
			 (condition-case nil
				 (progn
				   (set-buffer buffer)
				   (hi-lock-unface-buffer jda-highlight-symbol-symbol-regex))
			   (error nil))))
		 (message "jda-highlight-symbol off.")
		 (setq jda-highlight-symbol-timer nil))
		(t
		 (unless hi-lock-mode (hi-lock-mode 1))
		 (setq jda-highlight-symbol-timer
			   (run-with-idle-timer jda-highlight-symbol-timer-interval t 'jda-highlight-symbol-callback))
		 (message "jda-highlight-symbol on."))))

(provide 'jda-highlight)

;;; jda-highligt.el ends here
