;; ======================================================================
;; common functions
;; ======================================================================
(defun j-icompleting-read (prompt choices)
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))

(defun j-read-shell-command (prompt initial-contents &optional history)
  (if (functionp 'read-shell-command)
	  (read-shell-command prompt
						  initial-contents
						  history)
	(read-from-minibuffer prompt
						  initial-contents
						  nil
						  nil
						  history)))

(defun j-is-directory(path)
  (car (file-attributes path)))

(defun j-get-super-directory(path)
  (replace-regexp-in-string "/[^/]*$"
							""
							path))

(defmacro j-set-default-directory(prompt var var-history)
  `(progn (if (null ,var)
			  (setq ,var default-directory))
		  (setq ,var
				(completing-read ,prompt
								 'ffap-read-file-or-url-internal
								 nil
								 nil
								 ,var
								 ,var-history))))

;; ======================================================================
;; utility functions
;; ======================================================================
(defun j-add-new-line-to-eof()
  "버퍼 마지막에 new line이 없으면 추가한다."
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(if (equal (looking-back "\n") nil)
		(insert "\n"))))

(defun j-is-already-exist-if-def-pragma-once()
  "#pragma once가 이미 #ifdef OS_WIN ~ #endif안에 있는지 체크한다."
  (save-excursion
	(c-up-conditional 1)
	(looking-at "#[ \t\n]*ifdef[ \t\n]*OS_WIN")))

(defun j-add-if-def-pragma-once()
  "#pragma once를 #ifdef OS_WIN ~ #endif안에 있도록 수정한다."
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*#[ \t\n]*pragma[ \t\n]+once" nil t)
	  (beginning-of-line)
	  (kill-line)
	  (insert "#ifdef OS_WIN\n#\tpragma once\n#endif"))))

(defvar j-once-header-project-root-directory  nil)
(defvar j-once-header-project-root-directory-history nil)

(defun j-get-header-define-string()
  "header define 문자열을 생성하여 리턴한다."
  (interactive)
  (let (define-string)
	(setq define-string (buffer-file-name (current-buffer)))
	(j-set-default-directory "Project root directory: "
							 j-once-header-project-root-directory
							 'j-once-header-project-root-directory-history)
	(setq define-string (replace-regexp-in-string j-once-header-project-root-directory
												  ""
												  define-string))
	(setq define-string (upcase (replace-regexp-in-string "[/\\.]"
														  "_"
														  define-string)))))

(defun j-is-already-exist-if-def-header(header-define-name)
  "#define header-define-name 이 존재하는지 체크한다.
존재하면 t 그렇지 않으면 nil리턴한다."
  (interactive)
  (save-excursion
	(let (define-header-name-regexp)
	  (goto-char (point-min))
	  (setq define-header-name-regexp (format "^[ \t]*#[ \t\n]*define[ \t\n]+%s"
											  header-define-name))
	  (re-search-forward  define-header-name-regexp nil t))))

(defun j-add-ifndef-header-define-name()
  "#ifndef header_file_h #define header_file_h ~ #endif가 없으면 추가한다."
  (interactive)
  (save-excursion
	(let (header-define-name new-line count-of-new-line) 
	  (setq header-define-name (j-get-header-define-string))
	  (if (equal (j-is-already-exist-if-def-header header-define-name) nil)
		  (progn
			(goto-char (point-min))
			(insert (format "#ifndef %s\n#define %s\n\n"
							header-define-name
							header-define-name))
			(goto-char (point-max))
			(setq count-of-new-line 0)
			(while (and (looking-back "\n") (not (bobp)))
			  (setq count-of-new-line (+ 1 count-of-new-line))
			  (forward-char -1))
			(cond ((>= count-of-new-line 2)
				   ;; new line이 필요없는 경우 삭제한다.
				   (delete-char (- count-of-new-line 2))
				   (setq new-line ""))
				  ((equal count-of-new-line 1)
				   (setq new-line "\n"))
				  (t
				   (setq new-line "\n\n")))
			(goto-char (point-max))
			(insert new-line)
			(insert (format "#endif // %s\n" header-define-name)))))))

(defun j-modify-header-file-for-g++()
  "g++ warning제거하기 위해 마지막줄에
new line, #ifndef ~, #ifdef OS_WIN #pragma once ~을 header file에 추가한다."
  (interactive)
  (j-add-ifndef-header-define-name)
  (j-add-if-def-pragma-once))

(defun j-get-makefile-dir()
  "Makefile 존재 디렉토리 알려준다.

현재 버퍼의 위치를 기준으로 Makefile이 존재하는지 체크하여
존재하면 존재하는 Makefile파일의 디렉토리를 리턴한다.
존재하지 않으면 계속 상위 디렉토리에서 조사하여 root까지 조사한다."
  (interactive)
  (let (makefile-dir)
	(setq makefile-dir (buffer-file-name))
	(if (equal makefile-dir nil)
		(setq makefile-dir default-directory) ; not filebuffer
	  (setq makefile-dir (j-get-super-directory makefile-dir)))
	(while (and (not (equal makefile-dir ""))
				(not (file-exists-p (concat makefile-dir "/Makefile"))))
	  (setq makefile-dir (j-get-super-directory makefile-dir)))
	makefile-dir))

(defvar j-make-command-history nil)
(defun j-make ()
  "Makefile파일 위치를 찾아 컴파일 명령 문자열을 자동으로 생성
예) make -C <DIR>"
  (interactive)
  (let (compile-string makefile-dir)
	(setq makefile-dir (j-get-makefile-dir))
	(if (equal makefile-dir "")
		(setq compile-string "make ")
	  (setq compile-string (format "make -C %s " makefile-dir)))
	(setq compile-string (read-from-minibuffer "Compile command: "
											   compile-string
											   nil
											   nil
											   'j-make-command-history))
	(compile compile-string)))

(defun j-get-sub-directory-list(dir)
  (let ((entries (directory-files dir t))
		(sub-dirs '()))
	(mapcar (lambda (entry) (if (j-is-directory entry)
							(add-to-list 'sub-dirs entry t)))
			entries)
	sub-dirs))

(defun j-get-extensions-to-visit()
  (let (extension extensions-to-visit)
	(if (null (buffer-file-name))
		(setq extensions-to-visit nil)
	  (progn
		(setq extension (file-name-extension (buffer-file-name)))
		(cond ((equal extension "c")
			   (setq extensions-to-visit '("h")))
			  ((equal extension "cpp")
			   (setq extensions-to-visit '("h")))
			  ((equal extension "m")
			   (setq extensions-to-visit '("h")))
			  ((equal extension "mm")
			   (setq extensions-to-visit '("h")))
			  ((equal extension "h")
			   (setq extensions-to-visit '("c" "cpp" "m" "mm")))
			  (t
			   (setq extensions-to-visit nil)))))))

(defun j-visit-file(file-name-sans-ext extensions)
  (let (file-name file-ext)
	(while (not (equal (setq file-ext (pop extensions)) nil))
	  (setq file-name (concat file-name-sans-ext "." file-ext))
	  (if (file-exists-p file-name)
		  (progn
			(find-file file-name)
			(throw 'visit-file-exception file-name))))))

(defun j-visit-file-in-sub-dirs(sub-dir-list file-name-non-dir extensions)
  (let (file-name-sans-ext)
	(mapcar (lambda (entry)
			  (setq file-name-sans-ext (concat entry "/" file-name-non-dir))
			  (j-visit-file file-name-sans-ext extensions))
			sub-dir-list)))

(defun j-visit-file-in-dirs(super-dir-depth sub-dir-depth)
  (let ((extensions-to-visit (j-get-extensions-to-visit))
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
	(setq sub-dir-list (j-get-sub-directory-list current-dir))
	(dotimes (i super-dir-depth)
	  (setq current-dir (j-get-super-directory current-dir))
	  (if (equal current-dir "")
		  (throw 'visit-file-exception "Not found!"))
	  (setq sub-dir-list (j-get-sub-directory-list current-dir))
	  (j-visit-file-in-sub-dirs sub-dir-list
								file-name-non-dir
								extensions-to-visit))
	
	;; in sub-dir of sub-dir
	;; ...
	(setq current-dir (file-name-directory file-name-sans-ext))
	(setq sub-dir-list (j-get-sub-directory-list current-dir))
	(let ((sub-dir-all-list nil))
	  (dotimes (i sub-dir-depth)
		(mapcar (lambda (entry)
				  (let ((dirs (j-get-sub-directory-list entry)))
					(mapcar (lambda (x)
							  (add-to-list 'sub-dir-all-list x t))
							dirs))
				  entry)
				sub-dir-list)
		(setq sub-dir-list sub-dir-all-list))
	  (j-visit-file-in-sub-dirs sub-dir-all-list
								file-name-non-dir
								extensions-to-visit))
	
  (throw 'visit-file-exception "Not found!")))
						 
(defun j-visit-header-or-source-file()
  "현재 파일과 연관된 헤더/소스파일을 오픈한다."
  (interactive)
  (message (catch 'visit-file-exception
			 (j-visit-file-in-dirs 2 2))))

(defvar j-grep-find-symbol-history nil)
(defvar j-grep-find-symbol-command-history nil)
(defvar j-grep-find-file-history nil)
(defvar j-grep-find-file-command-history nil)
(defvar j-grep-find-default-directory nil)
(defvar j-grep-find-default-directory-history nil)
(defvar j-grep-find-ignore-path "*.git* *.svn* *.cvs* *~ *# *TAGS *cscope.out")
(defvar j-grep-find-ignore-path-history nil)

(defun j-get-find-path-options()
  (let (path-list path-option)
	(if (equal j-grep-find-ignore-path "")
		(setq path-option "")
	  (progn
		(setq path-list 
			  (mapcar (lambda (x) (format "-path '%s'" x))
					  (split-string j-grep-find-ignore-path)))
		(setq path-option (pop path-list))
		(while path-list
		  (setq path-option (concat path-option " -o " (pop path-list))))
		(setq path-option (concat "! \\( " path-option " \\)"))))))

(defun j-get-find-name-options()
  (let (name-option extension)
	(if (null (buffer-file-name))
		(setq name-option "")
	  (progn
		(setq extension (file-name-extension (buffer-file-name)))
		(cond ((or (equal extension "c") (equal extension "cpp") (equal extension "h"))
			   (setq name-option "\\( -name '*.[cChH]' -o -name '*.[cC][pP][pP]' \\)"))
			  ((or (equal extension "m") (equal extension "mm"))
			   (setq name-option "\\( -name '*.[cChH]' -o -name '*.[cC][pP][pP]' -o -name '*.mm' -o -name '*.m' \\)"))
			  ((equal extension "java")
			   (setq name-option "-name '*.java'"))
			  ((equal extension "el")
			   (setq name-option "-name '*.el'"))
			  ((equal extension "rb")
			   (setq name-option "-name '*.rb'"))
			  (t
			   (setq name-option "")))))))

(defun j-grep-find-set-ignore-path()
  (interactive)
  (setq j-grep-find-ignore-path
		(read-from-minibuffer "Ignore paths for j-grep-find: "
							  j-grep-find-ignore-path
							  nil
							  nil
							  'j-grep-find-ignore-path-history)))

(defun j-grep-find-set-default-directory()
  "grep-find할 디렉토리를 설정한다."
  (interactive)
  (j-set-default-directory "Directory for j-grep-find: "
						   j-grep-find-default-directory
						   'j-grep-find-default-directory-history))

(defun j-grep-find-select-grep-buffer(current-buffer msg)
  (condition-case nil
	  (progn
		(select-window (get-buffer-window current-buffer))
		(forward-line 4))
	(error nil)))

(add-to-list 'compilation-finish-functions 'j-grep-find-select-grep-buffer)

(defun j-grep-find-symbol-at-point()
  "현재 파일형식과 현재 커서의 심볼을 가지고 grep-find한다."
  (interactive)
  (let (symbol)
	(j-grep-find-set-default-directory)
	(setq symbol (symbol-at-point))
	(if (null symbol)
		(setq symbol ""))
	(setq symbol (read-from-minibuffer "Symbol to find: "
									   (format "%s" symbol)
									   nil
									   nil
									   'j-grep-find-symbol-history))
	(grep-find (j-read-shell-command "Run find (like this): "
									 (format "find -L %s -type f %s %s -print0 | xargs -0 grep -nH -e \"\\<%s\\>\""
											 j-grep-find-default-directory
											 (j-get-find-name-options)
											 (j-get-find-path-options)
											 symbol)
									 'j-grep-find-symbol-command-history))))

(defun j-grep-find-file()
  "파일이름에 해당하는 파일을 찾는다."
  (interactive)
  (let (file-name)
	(j-grep-find-set-default-directory)
	(setq file-name (read-from-minibuffer "File-name to find: "
										  nil
										  nil
										  nil
										  'j-grep-find-file-history))
	(grep-find (j-read-shell-command "Run find (like this): "
									 (format "find -L %s -type f -name '%s'"
											 j-grep-find-default-directory
											 file-name)
									 'j-grep-find-file-command-history))))

(defvar j-create-tags-command nil)
(defvar j-create-tags-command-history nil)
(defvar j-create-tags-directory nil)
(defvar j-create-tags-directory-history nil)

(defun j-create-tags()
  "TAGS파일 생성"
  (interactive)
  (j-set-default-directory "Create tags: "
						   j-create-tags-directory
						   'j-create-tags-directory-history)
  (shell-command (j-read-shell-command "Run find (like this): "
									   (format "find -L %s -type f %s %s -print | etags - -o %s/TAGS"
											   j-create-tags-directory
											   (j-get-find-name-options)
											   (j-get-find-path-options)
											   j-create-tags-directory)
									   'j-create-tags-command-history)
				 "*j-create-tag*"))


(defvar j-etags-tag-info-alist nil)

(defun j-etags-make-tag-info-alist(file)
  (goto-char (point-min))
  (when (re-search-forward (concat "\f\n" "\\(" file "\\)" ",") nil t)
    (let ((path (save-excursion (forward-line 1) (file-of-tag)))
		  tag-info)
	  (forward-line 1)
	  (while (not (or (eobp) (looking-at "\f")))
		(setq tag-info (save-excursion (etags-snarf-tag t)))
		(add-to-list 'j-etags-tag-info-alist tag-info t)
		(forward-line 1))
	  t)))

(defun j-etags-goto-tag-in-file()
  (interactive)
  (setq j-etags-tag-info-alist nil)
  (let ((file (buffer-file-name)))
	(save-excursion
	  (let ((first-time t)
			(gotany nil))
		(while (visit-tags-table-buffer (not first-time))
		  (setq first-time nil)
		  (if (j-etags-make-tag-info-alist file)
			  (setq gotany t)))
		(or gotany
			(error "File %s not in current tags tables" file))))
	(let ((tags (mapcar (lambda (x) (car x))
						j-etags-tag-info-alist))
		  line
		  tag-info)
	  (setq tag-info (assoc (j-icompleting-read "Goto tag in file: "
												tags)
							j-etags-tag-info-alist))
	  (setq line (car (cdr tag-info)))
	  (goto-line line))))

(define-key global-map (kbd "C-c m m") 'j-modify-header-file-for-g++)
(define-key global-map (kbd "C-c c") 'j-make)
(define-key global-map (kbd "C-c j p") 'j-visit-header-or-source-file)
(define-key global-map (kbd "C-c j s") 'j-grep-find-symbol-at-point)
(define-key global-map (kbd "C-c j f") 'j-grep-find-file)
(define-key global-map (kbd "C-c j d") 'j-grep-find-set-default-directory)
(define-key global-map (kbd "C-c j i") 'j-grep-find-set-ignore-path)
(define-key global-map (kbd "C-c j t") 'j-create-tags)
(define-key global-map (kbd "C-c j m") 'j-etags-goto-tag-in-file)
(define-key global-map (kbd "C-c j v") 'visit-tags-table)
(define-key global-map (kbd "C-c j .") 'tags-apropos)
(define-key global-map (kbd "C-c j h") 'hs-minor-mode)
