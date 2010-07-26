;;; j-highlight.el --- highlight symbol
;;
;; Copyright 2010 Lee Jong-Gyu<jglee1027@gmail.com>
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
;; 	 along with GNU Emacs; see the file COPYING.  If not, write to the
;; 	 Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; 	 Boston, MA 02111-1307, USA.
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
(defvar j-highlight-symbol-timer-interval 1.0)
(defvar j-highlight-symbol-timer nil)
(defvar j-highlight-symbol-color 'hi-red-b)
(defvar j-highlight-symbol-old-symbol-regex nil)

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
				  (not (string= symbol-regex j-highlight-symbol-old-symbol-regex)))
			 (hi-lock-unface-buffer j-highlight-symbol-old-symbol-regex)
			 (hi-lock-face-buffer symbol-regex
								  j-highlight-symbol-color)
			 (setq j-highlight-symbol-old-symbol-regex symbol-regex))))))

;; start function
(defun j-highlight-symbol-run-toggle ()
  (interactive)
  (cond ((timerp j-highlight-symbol-timer)
		 (cancel-timer j-highlight-symbol-timer)
		 (and j-highlight-symbol-old-symbol-regex
			  (hi-lock-unface-buffer j-highlight-symbol-old-symbol-regex))
		 (message "j-highlight-symbol off.")
		 (setq j-highlight-symbol-timer nil))
		(t
		 (unless hi-lock-mode (hi-lock-mode 1))
		 (setq j-highlight-symbol-timer
			   (run-with-idle-timer 1.5 t 'j-highlight-symbol-callback))
		 (message "j-highlight-symbol on."))))

(define-key global-map (kbd "C-x j h") 'j-highlight-symbol-run-toggle)

;; ======================================================================
;; Using ThingAtPoint and the Existing C-s C-w
;; 
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; ======================================================================
(defun isearch-yank-symbol-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'symbol))
  (isearch-yank-char (- (end-of-thing 'symbol) (beginning-of-thing 'symbol)))
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'isearch-yank-symbol-from-beginning 
							 'isearch-yank-word-or-char
							 isearch-mode-map))

(add-hook 'isearch-mode-hook
		  (lambda ()
			"Activate my customized Isearch word yank command."
			(substitute-key-definition 'isearch-yank-word-or-char 
									   'isearch-yank-symbol-from-beginning
									   isearch-mode-map)))
;;; j-highligt.el ends here
