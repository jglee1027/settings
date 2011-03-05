;;; jda.el --- Jong-Gyu Development Asisstant for Emacs
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
;;     (add-to-list 'load-path "/path/to/jda.el")
;;     (require 'jda)
;; 
;; * Major commands:
;;   See `jda-minor-keymap'

;;; Code:

(require 'cl)
(require 'imenu)
(require 'ido)

(defgroup jda nil
  "Jong-Gyu Development Assistant"
  :group 'applications
  :prefix "jda-")

(defvar jda-create-tags-command nil)
(defvar jda-create-tags-command-history nil)
(defvar jda-create-tags-directory nil)
(defvar jda-create-tags-directory-history nil)
(defvar jda-etags-tag-info-alist nil)
(defvar jda-get-extensions-alist '(("c"		. ("h"))
								   ("cpp"	. ("h"))
								   ("m"		. ("h"))
								   ("mm"	. ("h"))
								   ("h"		. ("c" "cpp" "m" "mm"))))
(defvar jda-gf-symbol-history nil)
(defvar jda-gf-symbol-command-history nil)
(defvar jda-gf-find-file-history nil)
(defvar jda-gf-find-file-command-history nil)
(defvar jda-gf-replace-file-history nil)
(defvar jda-gf-replace-file-command-history nil)
(defvar jda-gf-project-root nil)
(defvar jda-gf-project-root-history nil)
(defvar jda-gf-grep-query-command-history nil)
(defvar jda-gf-exclusive-path-history nil)
(defvar jda-gf-grep-query-replace-buffers-alist nil)
(defvar jda-ido-find-file-files-alist nil)
(defvar jda-ido-find-file-files-alist-root nil)
(defvar jda-mark-ring-max 20)
(defvar jda-mark-ring (make-ring jda-mark-ring-max))
(defvar jda-mark-ring-iterator -1)
(defvar jda-make-command-history nil)
(defvar jda-xcode-doc-text-history nil)

(defcustom jda-gf-assoc-extension-alist
  '(("c"		. "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
	("cpp"		. "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
	("h"		. "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
	("m"		. "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
	("mm"		. "*.[cChH] *.[cC][pP][pP] *.[mM] *.[mM][mM]")
	("java"		. "*.java")
	("el"		. "*.el")
	("rb"		. "*.rb"))
  "Counterpart extensions"
  :type 'alist
  :group 'jda)

(defcustom jda-gf-exclusive-path
  "*.git* *.svn* *.cvs* *.class *.obj *.o *.a *.so *~ *# *.cache *TAGS *cscope.out"
  "Paths to exclude while find command runs"
  :type 'string
  :group 'jda)

;; ======================================================================
;; common functions
;; ======================================================================

(defun jda-icompleting-read (prompt choices)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun jda-read-shell-command (prompt initial-contents &optional history)
  (if (functionp 'read-shell-command)
	  (read-shell-command prompt
						  initial-contents
						  history)
	(read-from-minibuffer prompt
						  initial-contents
						  nil
						  nil
						  history)))

(defun jda-is-directory(path)
  (equal (car (file-attributes path)) t))

(defun jda-get-super-directory(path)
  (replace-regexp-in-string "/[^/]*$"
							""
							path))

(defmacro jda-set-default-directory(prompt var var-history)
  `(progn (if (null ,var)
			  (setq ,var default-directory))
		  (setq ,var
				(file-name-as-directory
				 (completing-read ,prompt
								  'ffap-read-file-or-url-internal
								  nil
								  nil
								  ,var
								  ,var-history)))))

;; jump-to-register in register.el
(defun jda-mark-jump(marker)
  (cond
   ((and (consp marker) (frame-configuration-p (car marker)))
	(set-frame-configuration (car marker) (not delete))
	(goto-char (cadr marker)))
   ((and (consp marker) (window-configuration-p (car marker)))
	(set-window-configuration (car marker))
	(goto-char (cadr marker)))
   ((markerp marker)
	(or (marker-buffer marker)
		(error "That marker's buffer no longer exists"))
	(switch-to-buffer (marker-buffer marker))
	(goto-char marker))
   ((and (consp marker) (eq (car marker) 'file))
	(find-file (cdr marker)))
   ((and (consp marker) (eq (car marker) 'file-query))
	(or (find-buffer-visiting (nth 1 marker))
		(y-or-n-p (format "Visit file %s again? " (nth 1 marker)))
		(error "marker access aborted"))
	(find-file (nth 1 marker))
	(goto-char (nth 2 marker)))
   (t
	(error "marker doesn't contain a buffer position or configuration"))))

(defun jda-mark-push-marker()
  (interactive)
  (setq jda-mark-ring-iterator -1)
  (condition-case nil
	  (if (ring-empty-p jda-mark-ring)
		  (ring-insert jda-mark-ring (point-marker))
		(let ((last-marker (ring-ref jda-mark-ring (car jda-mark-ring)))
			  (curr-marker (point-marker)))
		  (cond ((not (equal last-marker curr-marker))
				 (ring-insert jda-mark-ring curr-marker)
				 (message "The current marker was saved")))))
	(error nil)))

(defun jda-mark-prev()
  (interactive)
  (cond ((equal jda-mark-ring-iterator -1)
		 (jda-mark-push-marker)
		 (setq jda-mark-ring-iterator 0)))
  (condition-case nil
	  (let* ((prev-iterator (mod (1+ jda-mark-ring-iterator)
								 (ring-length jda-mark-ring)))
			 (prev-marker (ring-ref jda-mark-ring prev-iterator)))
		(jda-mark-jump prev-marker)
		(setq jda-mark-ring-iterator prev-iterator)
		(message "jumped to previous marker"))
	(error nil)))

(defun jda-mark-next()
  (interactive)
  (condition-case nil
	  (let* ((next-iterator (mod (1- jda-mark-ring-iterator)
			  					 (ring-length jda-mark-ring)))
			 (next-marker (ring-ref jda-mark-ring next-iterator)))
		(jda-mark-jump next-marker)
		(setq jda-mark-ring-iterator next-iterator)
		(message "jumped to next marker"))
	(error nil)))

;; ======================================================================
;; utility functions
;; ======================================================================

(defun jda-get-makefile-dir()
  "return the directory(project root directory) where Makefile exist."
  (interactive)
  (let (makefile-dir)
	(setq makefile-dir (buffer-file-name))
	(if (equal makefile-dir nil)
		(setq makefile-dir default-directory) ; not filebuffer
	  (setq makefile-dir (jda-get-super-directory makefile-dir)))
	(catch 'while-exit
	  (while (not (or (equal makefile-dir "")
					  (equal makefile-dir "~")))
		(cond ((file-exists-p (concat makefile-dir "/Makefile"))
			   (throw 'while-exit makefile-dir)))
		(setq makefile-dir (jda-get-super-directory makefile-dir)))
	  (throw 'while-exit ""))))

(defun jda-make ()
  "make compile-string like following if the directory in which Makefile exist is found.
ex) make -C project/root/directory"
  (interactive)
  (let (compile-string makefile-dir)
	(setq makefile-dir (jda-get-makefile-dir))
	(if (equal makefile-dir "")
		(setq compile-string "make ")
	  (setq compile-string (format "make -C %s " makefile-dir)))
	(setq compile-string (read-from-minibuffer "Compile command: "
											   compile-string
											   nil
											   nil
											   'jda-make-command-history))
	(compile compile-string)))

(defun jda-get-sub-directory-list(dir)
  (let ((entries (directory-files dir t))
		(sub-dirs '()))
	(mapcar (lambda (entry) (if (jda-is-directory entry)
								(add-to-list 'sub-dirs entry t)))
			entries)
	sub-dirs))

(defun jda-get-extensions-to-visit()
  (let (extension extensions-to-visit)
	(cond ((null (buffer-file-name))
		   (setq extensions-to-visit nil))
		  (t
		   (setq extension (downcase (file-name-extension (buffer-file-name))))
		   (setq extensions-to-visit (cdr (assoc extension jda-get-extensions-alist)))))))

(defun jda-visit-file(file-name-sans-ext extensions)
  (let (file-name file-ext)
	(while (not (equal (setq file-ext (pop extensions)) nil))
	  (setq file-name (concat file-name-sans-ext "." file-ext))
	  (if (file-exists-p file-name)
		  (progn
			(find-file file-name)
			(throw 'visit-file-exception file-name))))))

(defun jda-visit-file-in-sub-dirs(sub-dir-list file-name-non-dir extensions)
  (let (file-name-sans-ext)
	(mapcar (lambda (entry)
			  (setq file-name-sans-ext (concat entry "/" file-name-non-dir))
			  (jda-visit-file file-name-sans-ext extensions))
			sub-dir-list)))

(defun jda-visit-file-in-dirs(super-dir-depth sub-dir-depth)
  (let ((extensions-to-visit (jda-get-extensions-to-visit))
		file-name-sans-ext
		file-name-non-dir
		current-dir
		sub-dir-list)
	
	(if (or (equal extensions-to-visit nil)
			(equal (buffer-file-name) nil))
		(throw 'visit-file-exception "Not supported!"))
	
	(setq file-name-sans-ext (file-name-sans-extension (buffer-file-name)))
	(setq file-name-non-dir (file-name-nondirectory file-name-sans-ext))
	
	;; in current directory
	;; in super directory
	;; in sub-dir of super-dir
	;; ...
	(setq current-dir (file-name-directory file-name-sans-ext))
	(setq sub-dir-list (jda-get-sub-directory-list current-dir))
	(dotimes (i super-dir-depth)
	  (setq current-dir (jda-get-super-directory current-dir))
	  (if (equal current-dir "")
		  (return))
	  (setq sub-dir-list (jda-get-sub-directory-list current-dir))
	  (jda-visit-file-in-sub-dirs sub-dir-list
								  file-name-non-dir
								  extensions-to-visit))
	
	;; in sub-dir of sub-dir
	;; ...
	(setq current-dir (file-name-directory file-name-sans-ext))
	(setq sub-dir-list (jda-get-sub-directory-list current-dir))
	(let ((sub-dir-all-list nil))
	  (dotimes (i sub-dir-depth)
		(mapcar (lambda (entry)
				  (let ((dirs (jda-get-sub-directory-list entry)))
					(mapcar (lambda (x)
							  (delq x sub-dir-all-list)
							  (add-to-list 'sub-dir-all-list x t))
							dirs))
				  entry)
				sub-dir-list)
		(setq sub-dir-list sub-dir-all-list))
	  (jda-visit-file-in-sub-dirs sub-dir-all-list
								  file-name-non-dir
								  extensions-to-visit))))

(defun jda-visit-file-in-project()
  (interactive)
  (let ((file-name-sans-ext (file-name-sans-extension (buffer-name)))
		(same-name-files-list nil)
		(extensions-to-visit (jda-get-extensions-to-visit))
		(same-name-files-count 0)
		file-name
		file-ext)
	
	(if (null jda-ido-find-file-files-alist)
		(throw 'visit-file-exception "Not found! Set project root directory"))

	(while (not (null (setq file-ext (pop extensions-to-visit))))
	  (setq file-name (concat file-name-sans-ext "." file-ext))
	  (cond ((not (null (assoc file-name jda-ido-find-file-files-alist)))
			 (mapcar (lambda (x)
					   (cond ((equal file-name (car x))
							  (add-to-list 'same-name-files-list
										   (car (cdr x)))
							  (setq same-name-files-count
									(1+ same-name-files-count)))))
					 jda-ido-find-file-files-alist)
			 (cond ((equal same-name-files-count 1)
					(throw 'visit-file-exception
						   (buffer-file-name
							(find-file (car same-name-files-list)))))
				   ((> same-name-files-count 1)
					(throw 'visit-file-exception
						   (buffer-file-name
							(find-file
							 (ido-completing-read "Find file: "
												  same-name-files-list)))))))))))

(defun jda-open-counterpart-file()
  "open the header or source file related with the current file."
  (interactive)
  (message (catch 'visit-file-exception
			 (jda-visit-file-in-dirs 2 2)
			 (jda-visit-file-in-project)
			 (throw 'visit-file-exception "Not found!"))))

(defun jda-gf-get-find-exclusive-path-options()
  (let (path-list path-option)
	(if (equal jda-gf-exclusive-path "")
		(setq path-option "")
	  (progn
		(setq path-list (mapcar (lambda (x) (format "-path '%s'" x))
								(split-string jda-gf-exclusive-path)))
		(setq path-option (pop path-list))
		(while path-list
		  (setq path-option (concat path-option " -o " (pop path-list))))
		(setq path-option (concat "! \\( " path-option " \\)"))))))

(defun jda-gf-get-find-name-options(files)
  (let (name-list name-option)
	(if (equal files "")
		(setq name-option "")
	  (progn
		(setq name-list (mapcar (lambda (x) (format "-name '%s'" x))
								(split-string files)))
		(setq name-option (pop name-list))
		(while name-list
		  (setq name-option (concat name-option " -o " (pop name-list))))
		(setq name-option (concat "\\( " name-option " \\)"))))))

(defun jda-gf-get-assoc-find-name-options()
  (let (name-option name-list extension assoc-extensions)
	(cond ((null (buffer-file-name))
		   (setq name-option (jda-gf-get-find-name-options
							  (read-from-minibuffer "Find file: "))))
		  (t
		   (setq extension (downcase (file-name-extension (buffer-file-name))))
		   (setq assoc-extensions (cdr (assoc extension jda-gf-assoc-extension-alist)))
		   (cond (assoc-extensions
				  (setq name-list (mapcar (lambda (x) (format "-name '%s'" x))
										  (split-string assoc-extensions)))
				  (setq name-option (pop name-list))
				  (while name-list
					(setq name-option (concat name-option " -o " (pop name-list))))
				  (setq name-option (concat "\\( " name-option " \\)")))
				 (t
				  (setq name-option "")))))))

(defun jda-gf-set-exclusive-path()
  (interactive)
  (setq jda-gf-exclusive-path
		(read-from-minibuffer "Exclusive paths: "
							  jda-gf-exclusive-path
							  nil
							  nil
							  'jda-gf-exclusive-path-history)))

(defun jda-gf-set-project-root()
  "set a project root directory for grep-find"
  (interactive)
  (jda-set-default-directory "Project root: "
							 jda-gf-project-root
							 'jda-gf-project-root-history))

(defun jda-gf-select-grep-buffer(current-buffer msg)
  (condition-case nil
	  (progn
		(select-window (get-buffer-window current-buffer))
		(forward-line 4)
		(setq compilation-finish-function nil))
	(error nil)))

(defun jda-gf-symbol-at-point()
  "grep-find with symbol at current point."
  (interactive)
  (jda-mark-push-marker)
  (let (symbol)
	(jda-gf-set-project-root)
	(setq symbol (symbol-at-point))
	(if (null symbol)
		(setq symbol ""))
	(setq symbol (read-from-minibuffer "Find symbol: "
									   (format "%s" symbol)
									   nil
									   nil
									   'jda-gf-symbol-history))
	(setq compilation-finish-function 'jda-gf-select-grep-buffer)
	(grep-find (jda-read-shell-command "Command: "
									   (format "find -L %s -type f %s %s -print0 | xargs -0 grep -nH -e \"\\<%s\\>\""
											   jda-gf-project-root
											   (jda-gf-get-assoc-find-name-options)
											   (jda-gf-get-find-exclusive-path-options)
											   symbol)
									   'jda-gf-symbol-command-history))))

(defun jda-gf-grep-query-replace-in-current-line(from to buffer)
  (let (begin end)
	(with-current-buffer buffer
	  (save-excursion
		(beginning-of-line)
		(setq begin (point))
		(end-of-line)
		(setq end (point)))
	  (replace-regexp from to nil begin end))))

(defun jda-gf-grep-query-replace-ui(from to &optional delimited)
  (interactive
   (let ((common
  		  (query-replace-read-args
  		   "Query replace regexp in found files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  
  (condition-case nil
	  (next-error)
	(error "Query replace finished"))
  (condition-case nil
	  (previous-error)
	(error nil))
  
  (let ((done nil)
		(all nil)
		(count 0)
		key
		buffer)
	(message "")
	
	(while (not done)
	  (with-current-buffer "*grep*"
		(setq buffer (get-buffer
					  (file-name-nondirectory
					   (car
						(car (nth 2 (car (compilation-next-error 0)))))))))

	  (with-current-buffer buffer
		(setq jda-gf-grep-query-replace-buffers-alist
			  (assq-delete-all buffer
							   jda-gf-grep-query-replace-buffers-alist))
		(add-to-list 'jda-gf-grep-query-replace-buffers-alist
					 (list buffer from) t)
		(hi-lock-face-buffer from 'query-replace))

	  (message (format "Query replacing '%s' with '%s' (y/n/a/q)?" from to))
	  (setq key (read-event))
	  
	  (cond ((equal key ?y)
			 (setq count (+ 1 count))
			 (jda-gf-grep-query-replace-in-current-line from to buffer)
			 (condition-case nil
				 (next-error)
			   (error
				(setq done t))))
			((equal key ?n)
			 (condition-case nil
				 (next-error)
			   (error
				(setq done t))))
			((equal key ?a)
			 (setq all t)
			 (setq done t))
			((equal key ?q)
			 (setq done t))))
	
	(setq done nil)
	(cond (all
		   (while (not done)
			 (jda-gf-grep-query-replace-in-current-line from to buffer)
			 (setq count (+ 1 count))
			 (condition-case nil
				 (next-error)
			   (error 
				(setq done t))))))

	(while jda-gf-grep-query-replace-buffers-alist
	  (let* ((entry (pop jda-gf-grep-query-replace-buffers-alist))
			 (buffer (pop entry))
			 (symbol-regex (pop entry)))
		(condition-case nil
			(progn
			  (set-buffer buffer)
			  (hi-lock-unface-buffer symbol-regex))
		  (error nil))))
	
	(message "Replaced %d occurrences" count)))

(defun jda-gf-grep-query-replace(from to &optional delimited)
  (interactive
   (let ((common
		  (query-replace-read-args
		   "Query replace regexp in files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  
  (jda-mark-push-marker)
  (jda-gf-set-project-root)
  (let (name-option
		command
		extension)
	(cond ((null (buffer-file-name))
		   (setq name-option ""))
		  (t
		   (setq extension (downcase
							(file-name-extension (buffer-file-name))))
		   (setq name-option (cdr (assoc
								   extension
								   jda-gf-assoc-extension-alist)))))
	(setq name-option (jda-gf-get-find-name-options
					   (read-from-minibuffer "Find file: "
											 name-option)))
	(setq command (jda-read-shell-command
				   "Command: "
				   (format "find -L %s -type f %s %s -print0 | xargs -0 grep -nH -e \"%s\""
						   jda-gf-project-root
						   name-option
						   (jda-gf-get-find-exclusive-path-options)
						   from)
				   'jda-gf-grep-query-command-history))
	
	(shell-command command "*grep*")
	(with-current-buffer "*grep*"
	  (grep-mode)
	  (hi-lock-face-buffer from
						   'match))
	(jda-gf-grep-query-replace-ui from to)))

(defun jda-gf-find-file()
  "search a file."
  (interactive)
  (jda-mark-push-marker)
  (let (files)
	(jda-gf-set-project-root)
	(setq files (read-from-minibuffer "Find file: "
									  nil
									  nil
									  nil
									  'jda-gf-find-file-history))
	(setq compilation-finish-function 'jda-gf-select-grep-buffer)
	(grep-find (jda-read-shell-command "Command: "
									   (format "find -L %s -type f %s "
											   jda-gf-project-root
											   (jda-gf-get-find-name-options files))
									   'jda-gf-find-file-command-history))))

(defun jda-gf-get-query-replace-files()
  (let ((files nil))
	(with-current-buffer "*jda-query-replace*"
	  (let (start end)
		(goto-char (point-min))
		(while (not (eobp))
		  (setq start (point))
		  (forward-line 1)
		  (setq end (- (point) 1))
		  (add-to-list 'files (buffer-substring start end) t)
		  (setq start (point)))))
	
	(dolist (file files)
	  (let ((buffer (get-file-buffer file)))
		(if (and buffer (with-current-buffer buffer
						  buffer-read-only))
			(error "File `%s' is visited read-only" file))))
	files))

(defun jda-gf-find-query-replace(from to &optional delimited)
  (interactive
   (let ((common
		  (query-replace-read-args
		   "Query replace regexp in files" t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (jda-mark-push-marker)
  (let (files)
	(jda-gf-set-project-root)
	
	(cond ((null (buffer-file-name))
		   (setq files ""))
		  (t
		   (setq files (cdr (assoc (downcase (file-name-extension (buffer-file-name)))
								   jda-gf-assoc-extension-alist)))))

	(setq files (read-from-minibuffer "Query replace file: "
									  files
									  nil
									  nil
									  'jda-gf-replace-file-history))
	(shell-command (jda-read-shell-command "Command: "
										   (format "find -L %s -type f %s "
												   jda-gf-project-root
												   (jda-gf-get-find-name-options files))
										   'jda-gf-replace-file-command-history)
				   "*jda-query-replace*")
	(delete-other-windows)
	(condition-case err
		(tags-query-replace from to delimited '(jda-gf-get-query-replace-files))
	  (error 
	   (kill-buffer "*jda-query-replace*")
	   (message "%s" (error-message-string err))))))

(defun jda-create-tags()
  "create TAG file."
  (interactive)
  (jda-set-default-directory "Create tags: "
							 jda-create-tags-directory
							 'jda-create-tags-directory-history)
  (shell-command (jda-read-shell-command "Command: "
										 (format "find -L %s -type f %s %s -print | etags - -o %sTAGS"
												 jda-create-tags-directory
												 (jda-gf-get-assoc-find-name-options)
												 (jda-gf-get-find-exclusive-path-options)
												 jda-create-tags-directory)
										 'jda-create-tags-command-history)
				 "*jda-create-tag*"))

(defun jda-etags-make-tag-info-alist(file)
  (goto-char (point-min))
  (when (re-search-forward (concat "\f\n" "\\(" file "\\)" ",") nil t)
    (let ((path (save-excursion (forward-line 1) (file-of-tag)))
		  tag-info)
	  (forward-line 1)
	  (while (not (or (eobp) (looking-at "\f")))
		(setq tag-info (save-excursion (etags-snarf-tag t)))
		(add-to-list 'jda-etags-tag-info-alist tag-info t)
		(forward-line 1))
	  t)))

(defun jda-etags-goto-tag-in-file()
  (interactive)
  (setq jda-etags-tag-info-alist nil)
  (let ((file (buffer-file-name)))
	(save-excursion
	  (let ((first-time t)
			(gotany nil))
		(while (visit-tags-table-buffer (not first-time))
		  (setq first-time nil)
		  (if (jda-etags-make-tag-info-alist file)
			  (setq gotany t)))
		(or gotany
			(error "File %s not in current tags tables" file))))
	(let ((tags (mapcar (lambda (x) (car x))
						jda-etags-tag-info-alist))
		  line
		  tag-info)
	  (setq tag-info (assoc (jda-icompleting-read "Goto tag in file: "
												  tags)
							jda-etags-tag-info-alist))
	  (setq line (car (cdr tag-info)))
	  (goto-line line))))

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(defun jda-ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
	(let ((enable-recursive-minibuffers t))
	  (visit-tags-table-buffer))
	(find-file
	 (expand-file-name
	  (ido-completing-read
	   "Project file: " (tags-table-files) nil t)))))

;; http://www.emacswiki.org/cgi-bin/wiki/ImenuMode
(defun jda-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the current buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
	(require 'imenu nil t))
  (cond
   ((not symbol-list)
	(let ((ido-mode ido-mode)
		  (ido-enable-flex-matching
		   (if (boundp 'ido-enable-flex-matching)
			   ido-enable-flex-matching t))
		  name-and-pos symbol-names position)
	  (unless ido-mode
		(ido-mode 1)
		(setq ido-enable-flex-matching t))
	  (while (progn
			   (imenu--cleanup)
			   (setq imenu--index-alist nil)
			   (jda-ido-goto-symbol (imenu--make-index-alist))
			   (setq selected-symbol
					 (ido-completing-read "Symbol? " symbol-names))
			   (string= (car imenu--rescan-item) selected-symbol)))
	  (unless (and (boundp 'mark-active) mark-active)
		(push-mark nil t nil))
	  (setq position (cdr (assoc selected-symbol name-and-pos)))
	  (cond
	   ((overlayp position)
		(goto-char (overlay-start position)))
	   (t
		(goto-char position)))))
   ((listp symbol-list)
	(dolist (symbol symbol-list)
	  (let (name position)
		(cond
		 ((and (listp symbol) (imenu--subalist-p symbol))
		  (jda-ido-goto-symbol symbol))
		 ((listp symbol)
		  (setq name (car symbol))
		  (setq position (cdr symbol)))
		 ((stringp symbol)
		  (setq name symbol)
		  (setq position
				(get-text-property 1 'org-imenu-marker symbol))))
		(unless (or (null position) (null name)
					(string= (car imenu--rescan-item) name))
		  (add-to-list 'symbol-names name)
		  (add-to-list 'name-and-pos (cons name position))))))))

(defun jda-goto-symbol()
  (interactive)
  (jda-mark-push-marker)
  (jda-ido-goto-symbol)
  (jda-mark-push-marker))

(defun jda-ido-find-file()
  (interactive)
  (jda-mark-push-marker)
  (let (chosen-name
		find-command
		same-name-files-list
		(same-name-files-count 0))
	(jda-gf-set-project-root)
	(setq find-command
		  (format "find -L %s -type f %s"
				  jda-gf-project-root
				  (jda-gf-get-find-exclusive-path-options)))
	(message "Finding...")
	;; if the previous project root directory equals to the current one,
	;; use the previous jda-ido-find-file-files-alist to improve speed.
	(cond ((not (equal jda-gf-project-root
					   jda-ido-find-file-files-alist-root))
		   (setq jda-ido-find-file-files-alist
				 (mapcar (lambda (x)
						   (list (file-name-nondirectory x) x))
						 (split-string
						  (shell-command-to-string find-command))))
		   (setq jda-ido-find-file-files-alist-root jda-gf-project-root)))
	(setq chosen-name
		  (ido-completing-read "Project file: "
							   (mapcar (lambda (x)
										 (car x))
									   jda-ido-find-file-files-alist)))
	(mapcar (lambda (x)
			  (cond ((equal chosen-name (car x))
					 (add-to-list 'same-name-files-list
								  (car (cdr x)))
					 (setq same-name-files-count
						   (1+ same-name-files-count)))))
			jda-ido-find-file-files-alist)
	(cond ((equal same-name-files-count 1)
		   (find-file (car same-name-files-list)))
		  ((> same-name-files-count 1)
		   (find-file (ido-completing-read "Find file: "
										   same-name-files-list))))))

(defun jda-svn-log-report()
  (interactive)
  (let (command
		id
		start-date
		end-date)
	(jda-gf-set-project-root)
	(setq id (read-from-minibuffer "Id: "))
	(setq start-date (read-from-minibuffer "Start date: "))
	(setq end-date (read-from-minibuffer "End date: "))
	(setq command (jda-read-shell-command
				   "Command: "
				   (format "cd %s; svn log | ~/settings/emacs/svnlr.rb -id %s -sd %s -ed %s"
						   jda-gf-project-root
						   id
						   start-date
						   end-date)))
	(shell-command command "*svn-log-report*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xcode-document-viewer
(condition-case nil
	(progn
	  (require 'w3m)
	  (require 'xcode-document-viewer)
	  (setq xcdoc:document-path "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone4_0.iPhoneLibrary.docset"))
  (error nil))

(defun jda-xcode-build()
  (interactive)
  (shell-command "~/settings/emacs/xcode-build"))

(defun jda-xcode-doc()
  (interactive)
  (let ((text (symbol-at-point))
		command)
	(if (null text)
		(setq text ""))
	(setq text (read-from-minibuffer "Find text in Xcode Doc: "
									 (format "%s" text)
									 nil
									 nil
									 'jda-xcode-doc-text-history))
	(setq command (format "~/settings/emacs/xcode-doc %s" text))
	(shell-command command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; android-doc
(defcustom jda-android-sdk-dir
  "~/android/android-sdk-linux_86"
  "Android SDK directory"
  :type 'directory
  :group 'jda)

(defun* jda-android-doc-source-candidates(&key class-name sdk-dir)
  (split-string
   (shell-command-to-string
	(format "find %s/docs/reference -name '*%s*'"
			sdk-dir
			class-name))))

(defun jda-android-doc-source()
  `((name . ,jda-android-sdk-dir)
	(candidates . (lambda ()
					(jda-android-doc-source-candidates
					 :class-name anything-pattern
					 :sdk-dir jda-android-sdk-dir)))
	(volatile)
	(delayed)
	(requires-pattern . 2)
	(action . browse-url)))

(defun jda-android-doc()
  (interactive)
  (let ((symbol (symbol-at-point)))
	(if (null symbol)
		(setq symbol "")
	  (setq symbol (symbol-name symbol)))
	(anything (list (jda-android-doc-source)) symbol)))

(defun jda-doc()
  (interactive)
  (unless (featurep 'anything)
	(require 'anything))
  (cond ((equal mode-name "ObjC/l")
		 (jda-xcode-doc))
		((equal mode-name "JDE")
		 (jda-android-doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; objc
(defun jda-insert-objc-parenthesis()
  (interactive)
  (let ((depth 0)
		(is-exist-left-bracket nil)
		(is-exist-right-bracket nil)
		(current-point (point))
		(beginning-of-defun-point nil)
		(should-insert-brakets nil))	; (left right)

	(c-beginning-of-defun)
	(setq beginning-of-defun-point (point))
	(goto-char current-point)
	
	(setq is-exist-right-bracket (looking-back "\\][ \t\n]*"))
	
	;; search parentheses and move the position
	(setq should-insert-brakets
		  (catch 'while-exit
			(while t
			  (backward-char 1)
			  (cond ((>= beginning-of-defun-point
						 (point))
					 (throw 'while-exit '(nil nil)))
					((looking-at "\\[")
					 (setq depth (1+ depth))
					 (setq is-exist-left-bracket t))
					((looking-at "\\]")
					 (setq depth (1- depth))))

			  (cond ((and (equal depth 0)
						  (or (looking-back "[={};&|][ \t\n]*")
							  (looking-back "\\(if\\|switch\\|while\\)[ \t\n]*(")))
					 (if (and is-exist-left-bracket
							  is-exist-right-bracket)
						 (throw 'while-exit '(nil nil))
					   (throw 'while-exit '(t t))))
					((equal depth 1)
					 (throw 'while-exit '(nil t)))))))
	
	;; insert "["
	(cond ((and (equal depth 0)
				(< beginning-of-defun-point
				   (point))
				(not (looking-back "\\[[ \t\n]*"))
				(car should-insert-brakets))
		   (insert "[")
		   (setq current-point (1+ current-point))))

	(goto-char current-point)
	(if (nth 1 should-insert-brakets)
		(insert "]"))))

(defun jda-delete-objc-parenthesis()
  (interactive)
  (cond ((looking-back "\\][ \t\n]*")
		 (let ((left-bracket-pos nil)
			   (right-bracket-pos nil))
		   (backward-list)
		   (setq left-bracket-pos (point))
		   (forward-list)
		   (setq right-bracket-pos (point))
		   
		   (goto-char left-bracket-pos)
		   (delete-char 1)
		   
		   (goto-char right-bracket-pos)
		   (backward-char 2)
		   (delete-char 1)))))

(defun jda-objc-keymap()
  (define-key objc-mode-map (kbd "C-c [") 'jda-delete-objc-parenthesis)
  (define-key objc-mode-map (kbd "C-c ]") 'jda-insert-objc-parenthesis))
  
;; ======================================================================
;; jda-minor mode functions
;; ======================================================================

(defun jda-minor-keymap()
  (let ((map (make-sparse-keymap)))
	(easy-menu-define jda-minor-mode-menu
	  map
	  "Menu used when jda-minor-mode is ative."
	  '("JDA"
		["Make..." jda-make
		 :help "Run make command"]
		["Find Doc.." jda-doc
		 :help "Find documentation for a symbol"]
		["Open counterpart file" jda-open-counterpart-file
		 :help "Open a counterpart file(.h .c .cpp .m .mm)"]
		"----"
		["Set Project Root Directory..." jda-gf-set-project-root
		 :help "Set a project root directory"]
		["Set Exclusive Path..." jda-gf-set-exclusive-path
		 :help "set exclusive paths"]
		["Find Symbol in project..." jda-gf-symbol-at-point
		 :help "Find a symbol in the project"]
		["Find File in project..." jda-gf-find-file
		 :help "Find a file in the proejct"]
		["Find Project File..." jda-ido-find-file
		 :help "Find a file in the proejct using incremental search"]
		["Goto Symbol in current buffer..." jda-goto-symbol
		 :help "Goto a symbol in the current buffer"]
		["Query Replace in the proejct..." jda-gf-grep-query-replace
		 :help "Query replace in the project using *grep* buffer"]
		["Query Replace in the project(Built-In)..." jda-gf-find-query-replace
		 :help "Query replace in the proejct using built-in query replace"]
		"----"
		["Create TAGS..." jda-create-tags
		 :help "Create TAGS file"]
		["Visit TAGS..." visit-tags-table
		 :help "Visit a TAGS table file"]
		["Display All tags regexp matches..." tags-apropos
		 :help "Display list of all tags in tags table REGEXP magtches"]
		"----"
		["Goto Previous Marker" jda-mark-prev
		 :help "Goto the previous marker"]
		["Goto Next Marker" jda-mark-next
		 :help "Goto the next marker"]
		["Push Current Marker" jda-mark-push-marker
		 :help "Push the current marker"]
		"---"
		("Objective-C"
		 ["Insert ']'" jda-insert-objc-parenthesis
		  :help "Insert ']' bracket parenthetically"
		  :active (equal mode-name "ObjC/l")]
		 ["Delete '['" jda-delete-objc-parenthesis
		  :help "Delete '[' bracket parenthetically"
		  :active (equal mode-name "ObjC/l")]
		 ["Xcode Build" jda-xcode-build
		  :help "Build the active target in Xcode"
		  :active (equal mode-name "ObjC/l")])
		["Align" align
		 :help "Align a region"]
		["Align Regexp..." align-regexp
		 :help "Align the current region using an ad-hoc rule"]
		["Report svn log..." jda-svn-log-report
		 :help "Report svn log using a condition"]
		["hs-minor-mode" hs-minor-mode
		 :help "hs-minor-mode on/off"]
		"----"
		["Customize JDA" jda-customize
		 :help "Customize jda-minor-mode"]
		["About JDA" jda-about
		 :help "Display some information about JDA"]))
	
	;; key map
	(define-key map (kbd "C-c c")		'jda-make)
	(define-key map (kbd "C-c h")		'jda-doc)
	(define-key map (kbd "C-c j p")		'jda-open-counterpart-file)
	(define-key map (kbd "C-c j r")		'jda-gf-set-project-root)
	(define-key map (kbd "C-c j e")		'jda-gf-set-exclusive-path)
	(define-key map (kbd "C-c j s")		'jda-gf-symbol-at-point)
	(define-key map (kbd "C-c j f")		'jda-gf-find-file)
	(define-key map (kbd "C-c j i")		'jda-ido-find-file)
	(define-key map (kbd "C-c i")		'jda-ido-find-file)
	(define-key map (kbd "C-c j m")		'jda-goto-symbol)
	(define-key map (kbd "C-c m")		'jda-goto-symbol)
	(define-key map (kbd "C-c j 5")		'jda-gf-grep-query-replace)
	(define-key map (kbd "C-c j %")		'jda-gf-find-query-replace)
	(define-key map (kbd "C-c j t")		'jda-create-tags)
	(define-key map (kbd "C-c j v")		'visit-tags-table)
	(define-key map (kbd "C-c j .")		'tags-apropos)
	(define-key map (kbd "C-x ,")		'jda-mark-prev)
	(define-key map (kbd "C-x .")		'jda-mark-next)
	(define-key map (kbd "C-x <down>")	'jda-mark-push-marker)
	(define-key map (kbd "C-c |")		'align)
	(define-key map (kbd "C-c M-|")		'align-regexp)
	(define-key map (kbd "C-x v #")		'jda-svn-log-report)
	(define-key map (kbd "C-c x b")		'jda-xcode-build)
	(define-key map (kbd "C-c j [")		'hs-minor-mode)
	map))
		
(defvar jda-minor-mode-map (jda-minor-keymap))

(define-minor-mode jda-minor-mode
  "Toggle Jong-Gyu Development Assistant mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix arugment turns off the mode.

Key bindings:
\\{jda-minor-mode-map}."
  ;; init-value
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " jda"
  ;; The minor mode bindings.
  :keymap jda-minor-mode-map
  :group 'jda
  :global t
  (cond (jda-minor-mode
		 ;; initialize
		 (ido-mode t)
		 (if (boundp 'ffap-bindings)
			 (ffap-bindings))
         (add-hook 'emulation-mode-map-alists 'yas/direct-keymaps)
		 (add-hook 'objc-mode-hook 'jda-objc-keymap)
		 (add-hook 'isearch-mode-hook 'jda-mark-push-marker)
		 (add-hook 'isearch-mode-end-hook 'jda-mark-push-marker)
		 (message "jda minor mode enabled"))
		(t
		 ;; finalize
		 (remove-hook 'objc-mode-hook 'jda-objc-keymap)
		 (remove-hook 'isearch-mode-hook 'jda-mark-push-marker)
		 (remove-hook 'isearch-mode-end-hook 'jda-mark-push-marker)
		 (remove-hook 'emulation-mode-map-alists 'yas/direct-keymaps)
		 (message "jda minor mode disabled"))))

(defun jda-customize()
  "Customize jda"
  (interactive)
  (customize-group 'jda))

(defun jda-about()
  "About JDA"
  (interactive)
  (message "jda-minor-mode(version 0.1.0) Jong-Gyu <jglee1027@gmail.com>"))

(provide 'jda)

;;; jda.el ends here
