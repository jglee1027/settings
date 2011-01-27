;; ======================================================================
;; General setting
;; ======================================================================
(add-to-list 'load-path "~/settings/emacs/jglee-lisp")
(add-to-list 'load-path "~/settings/emacs/site-lisp")
(let (old-default-directory)
  (setq old-default-directory default-directory)
  (setq default-directory "~/settings/emacs/site-lisp")
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	  (normal-top-level-add-subdirs-to-load-path))
  (setq default-directory old-default-directory))

;; tab
(setq c-basic-offset 4)
(setq default-tab-width 4)
(setq tab-stop-list
	  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76  80 84 88 92 96 100 104 108 112 116 120))
(setq scalable-fonts-allowed t)
(setq compilation-scroll-output t)

(setq-default transient-mark-mode t)
(show-paren-mode t)

(column-number-mode t)
(display-time-mode t)
(ffap-bindings)
(iswitchb-mode)
(ido-mode t)

(condition-case nil
	(progn
	  (require 'auto-complete)
	  (add-to-list 'ac-modes 'jde-mode)
	  (add-to-list 'ac-modes 'objc-mode)
	  (global-auto-complete-mode t))
  (error nil))

(setq semantic-load-turn-everything-on t)
(setq vc-make-backup-files t)

(when window-system
  (tool-bar-mode -1))
(unless window-system
  (menu-bar-mode -1))

;; Language setting
(defvar default-encoding
  (let (lang)
	(setq lang (getenv "LANG"))
	(if (null lang)
		(setq lang ""))
	(cond ((string-match "UTF-8" lang)
		   'utf-8)
		  ((string-match "EUC-KR" lang)
		   'euc-kr)
		  ((eq system-type 'windows-nt)
		   'euc-kr)
		  (t
		   'utf-8))))

(when enable-multibyte-characters
  (set-language-environment "Korean")
  
  (setq-default file-name-coding-system default-encoding)
  (setq default-korean-keyboard "3")
  (prefer-coding-system default-encoding)
  (set-default-coding-systems default-encoding)
  (set-keyboard-coding-system default-encoding)
  (set-selection-coding-system default-encoding)
  (setq-default sendmail-coding-system default-encoding)
  (when (boundp 'encoded-kbd-mode-map)
	(define-key encoded-kbd-mode-map [27] nil))
  (set-terminal-coding-system
   (cond ((eq default-encoding 'utf-8)
		  (setenv "LANG" "ko_KR.UTF-8")
		  'utf-8)
		 ((eq default-encoding 'euc-kr)
		  (setenv "LANG" "ko_KR.EUC-KR")
		  'korean-cp949))))
(unless (or enable-multibyte-characters window-system)
  (standard-display-european t)
  (set-input-mode (car (current-input-mode))
                  (nth 1 (current-input-mode))
                  0))

;; shell environment
(setq-default shell-cd-regexp nil)
(setq-default shell-pushd-regexp nil)
(setq-default shell-popd-regexp nil)

;; secure on shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; dired
(add-hook 'dired-load-hook
		  (function (lambda()
					  (load "dired-x" nil)
					  (setq dired-guess-shell-alist-user
							(list
							 '("\\.[aA][vV][iI]$" "mplayer")
							 '("\\.[mM][kK][vV]$" "mplayer")
							 '("\\.[mM][pP]4$" "mplayer")
							 '("\\.[wW][mM][vV]$" "mplayer"))))))

;; ======================================================================
;; Key definition
;; ======================================================================
(global-set-key [C-kanji] 'set-mark-command)
(global-set-key [?\S- ] 'toggle-input-method)
(define-key global-map (kbd "C-x RET s") 'decode-coding-region)
(define-key global-map (kbd "C-c C-s a") 'semantic-complete-analyze-inline)
(define-key global-map (kbd "C-c /") 'semantic-ia-complete-symbol-menu)
(define-key global-map (kbd "C-c c") 'compile)
(define-key global-map (kbd "C-c r") 'recompile)
(define-key global-map (kbd "C-c TAB") 'indent-relative)
(define-key global-map (kbd "C-x p") 'previous-buffer)
(define-key global-map (kbd "C-x n") 'next-buffer)

;; tags
(fset 'find-next-tag "\C-u\256")
(fset 'find-prev-tag "\C-u-\256")
(global-set-key (kbd "M-8") 'find-next-tag)
(global-set-key (kbd "M-7") 'find-prev-tag)

;; gud
(defun gud-mode-common-keys()
  (message ">>> run gud-mode-common-keys")
  (global-set-key [f5] 'gud-step)
  (global-set-key (kbd "C-<f5>") 'gud-stepi)
  (global-set-key [f6] 'gud-next)
  (global-set-key (kbd "C-<f6>") 'gud-nexti)
  (global-set-key [f7] 'gud-finish)
  (global-set-key [f8] 'gud-cont)
  (global-set-key [f12] 'gdb-many-windows)
  (global-set-key (kbd "M-<up>") 'gud-up)
  (global-set-key (kbd "M-<down>") 'gud-down))

(add-hook 'gdb-mode-hook 'gud-mode-common-keys)
(add-hook 'sdb-mode-hook 'gud-mode-common-keys)
(add-hook 'dbx-mode-hook 'gud-mode-common-keys)
(add-hook 'xdb-mode-hook 'gud-mode-common-keys)
(add-hook 'perldb-mode-hook 'gud-mode-common-keys)
(add-hook 'pdb-mode-hook 'gud-mode-common-keys)
(add-hook 'jdb-mode-hook 'gud-mode-common-keys)

;; hs-minor-mode
(add-hook 'hs-minor-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c h '") 'hs-toggle-hiding)
			(local-set-key (kbd "C-c h ;") 'hs-hide-level)
			(local-set-key (kbd "C-c h h") 'hs-hide-block)
			(local-set-key (kbd "C-c h j") 'hs-show-block)
			(local-set-key (kbd "C-c h k") 'hs-hide-all)
			(local-set-key (kbd "C-c h l") 'hs-show-all)))

;; highlight-symbol
(condition-case nil
	(progn
	  (require 'highlight-symbol)
	  (global-set-key (kbd "C-c j 8") 'highlight-symbol-mode)
	  (global-set-key (kbd "C-c 8") 'highlight-symbol-at-point)
	  (global-set-key (kbd "M-p") 'highlight-symbol-prev)
	  (global-set-key (kbd "M-n") 'highlight-symbol-next))
  (error nil))

;; winmove
(condition-case nil
	(progn
	  (require 'windmove)
	  (global-set-key (kbd "C-x w h") 'windmove-left)
	  (global-set-key (kbd "C-x w j") 'windmove-down)
	  (global-set-key (kbd "C-x w k") 'windmove-up)
	  (global-set-key (kbd "C-x w l") 'windmove-right)
	  (windmove-default-keybindings))
  (error nil))

;; ======================================================================
;; Mode line and minibuffer
;; ======================================================================
(setq display-time-string-forms
	  '((if	(and
			 (not display-time-format)
			 display-time-day-and-date)
			(decode-coding-string (format-time-string "%m/%d (%a) " now)
								  (car default-process-coding-system))
		  "")
		(decode-coding-string 
		 (format-time-string
		  (or display-time-format
			  (if display-time-24hr-format "%H:%M" "%p %-I:%M"))
		  now)
		 (car default-process-coding-system))
		load
		(if mail " Mail" "")))

(setq display-time-day-and-date t)
(display-time)

;; ======================================================================
;; Programming modes
;; ======================================================================
(condition-case nil
	(progn
	  (require 'ruby-mode)
	  (require 'rdebug)
	  (require 'rubydb)
	  (require 'inf-ruby)
	  (require 'ruby-electric))
  (error nil))

;; rdebug keys
(defun rdebug-keys (map)
  (define-key map [f5] 'rdebug-step)
  (define-key map (kbd "C-<f5>") 'gud-stepi)
  (define-key map [f6] 'rdebug-next)
  (define-key map (kbd "C-<f6>") 'gud-nexti)
  (define-key map [f7] 'gud-finish)
  (define-key map [f8] 'gud-cont)
  (define-key map [f12] 'rdebug-restore-debugger-window-layout)
  (define-key map (kbd "M-<up>") 'gud-up)
  (define-key map (kbd "M-<down>") 'gud-down)
  (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled))
(setq rdebug-populate-common-keys-function 'rdebug-keys)

;; auto-mode-alist
(setq magic-mode-alist nil)				; to use eruby-nxhtml-mumamo-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cflow$" . cflow-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))

;; choose header file mode
(defun header-file-mode-hook()
  (if (and (file-name-extension buffer-file-name)
		   (string-match "[hH]"(file-name-extension buffer-file-name)))
	  (let ((filename (file-name-sans-extension buffer-file-name))
			(mode-alist '((".c" . c-mode)
						  (".C" . c-mode)
						  (".cpp" . c++-mode)
						  (".CPP" . c++-mode)
						  (".m" . objc-mode)
						  (".M" . objc-mode)
						  (".mm" . objc-mode)
						  (".MM" . objc-mode)))
			(mode nil))
		(dolist (ext-mode mode-alist)
		  (if (file-exists-p (concat filename (car ext-mode)))
			  (setq mode ext-mode)))
		  
		(if mode
			(funcall (cdr mode)))
  )))
	
(add-hook 'find-file-hook 'header-file-mode-hook)

(c-add-style
 "java"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset 0 . 0)
   (c-hanging-comment-starter-p)
   (c-offsets-alist
    (topmost-intro-cont . +)
    (statement-block-intro . +)
    (substatement-open . 0)
    (substatement . +)
    (inline-open . 0)
    (label . 0)
    (statement-case-open . +)
    (statement-cont . +)
    (knr-argdecl-intro . 5)
    (arglist-intro . +)
    (arglist-close . c-lineup-arglist)
    (brace-list-entry . 0)
    (access-label . 0)
    (inher-cont . c-lineup-java-inher)
    (func-decl-cont . c-lineup-java-throws))))

(add-hook 'c-mode-common-hook
		  (function (lambda ()
					  (setq abbrev-mode nil))))

(add-hook 'java-mode-hook
		  (function (lambda ()
					  (c-set-style "java"))))

(add-hook 'c-mode-hook
		  (function (lambda ()
					  (c-set-style "stroustrup"))))

(add-hook 'c++-mode-hook
		  (function (lambda ()
					  (c-set-style "stroustrup"))))

(add-hook 'idl-mode-hook
		  (function (lambda ()
					  (c-set-style "stroustrup"))))

;; JDE
;; Copy `/usr/local/share/emacs/20.3/site-lisp/jde-2.1.3/jtags*'to `/usr/local/bin/'
(condition-case nil
	(progn
	  (require 'jde)
	  (add-hook 'jde-mode-hook
				(lambda()
				  (local-set-key (kbd "C-c C-v .") 'jde-complete-minibuf))))
  (error nil))

;; Android
(condition-case nil
 	(require 'android)
  (error nil))

;; nxhtml
(condition-case nil
	(require 'autostart)
  (error nil))

;; geben
(condition-case nil
	(progn
	  (require 'geben)
	  (define-key geben-mode-map [f5] 'geben-step-into)
	  (define-key geben-mode-map [f6] 'geben-step-over)
	  (define-key geben-mode-map [f7] 'geben-step-out)
	  (define-key geben-mode-map [f8] 'geben-run))
  (error nil))

;; javascript
(condition-case nil
	(require 'javascript)
  (error nil))

;; ruby
(defun which-gem-package(package)
  (let (start end path version)
	(catch 'which-gem-exception
	  (shell-command (format "gem list -d %s" package) "*which-gem-package*")
	  (with-current-buffer "*which-gem-package*"
		(goto-char (point-min))
		(if (null (re-search-forward "Installed at: " nil t))
			(throw 'which-gem-exception nil))
		(setq start (point))
		(if (null (re-search-forward "$" nil t))
			(throw 'which-gem-exception nil))
		(setq end (point))
		(setq path (buffer-substring start end))

		(goto-char (point-min))
		(if (null (re-search-forward (format "%s \(" package) nil t))
			(throw 'which-gem-exception nil))
		(setq start (point))
		(if (null (re-search-forward "\)$" nil t))
			(throw 'which-gem-exception nil))
		(setq end (1- (point)))
		(setq version (buffer-substring start end))

		(setq path (format "%s/gems/%s-%s" path package version))))
	(kill-buffer "*which-gem-package*")
	path))

(condition-case nil
	(progn
	  (setq ri-ruby-script
			(expand-file-name "~/settings/emacs/site-lisp/ri-emacs/ri-emacs.rb"))
	  (load-library "~/settings/emacs/site-lisp/ri-emacs/ri-ruby.el")
	  (add-hook 'ruby-mode-hook
				(lambda()
				  (local-set-key (kbd "C-c h") 'ri)
				  (local-set-key (kbd "C-c @") 'ri-ruby-show-args))))
  (error nil))

(let ((rcodetools-path (which-gem-package "rcodetools")))
  (condition-case nil
	  (cond ((not (null rcodetools-path))
			 (add-to-list 'load-path rcodetools-path)
			 (require 'anything-rcodetools)
			 (define-key ruby-mode-map (kbd "C-c /") 'rct-complete-symbol)))
	(error nil)))

;; rinari
(condition-case nil
	(require 'rinari)
  (error nil))

;; ======================================================================
;; Cscope
;; ======================================================================
(condition-case nil
	(progn
	  (require 'xcscope)
	  (add-hook 'java-mode-hook (function cscope:hook))
	  (add-hook 'asm-mode-hook (function cscope:hook))
	  (add-hook 'c-mode-common-hook (function cscope:hook)))
  (error nil))

(autoload 'cflow-mode "cflow-mode")

;; Source Navigator
(condition-case nil
    (require 'sn)
  (error nil))

;; ======================================================================
;; CEDET and ECB
;; ======================================================================
(condition-case nil
	(progn
	  (require 'cedet)
	  (require 'ecb))
  (error nil))

;; ======================================================================
;; doxymacs
;; ======================================================================
(condition-case nil
	(progn
	  (require 'doxymacs)
	  (add-hook 'c-mode-common-hook 'doxymacs)
	  (add-hook 'java-mode-hook 'doxymacs))
  (error nil))

;; ======================================================================
;; dsvn
;; ======================================================================
(autoload 'svn-status "dsvn" "Run 'svn status'." t)
(autoload 'svn-update "dsvn" "Run 'svn update'." t)
	  
;; ======================================================================
;; yasnippet
;; ======================================================================
(condition-case nil
	(progn
	  (require 'yasnippet)
	  (yas/initialize)
	  (yas/load-directory "~/settings/emacs/site-lisp/yasnippet/snippets")
	  (yas/load-directory "~/settings/emacs/snippets"))
  (error nil))

;; ======================================================================
;; Org-mode
;; ======================================================================
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a@)" "|" "DONE(d!)" "CANCELED(c@)" "POSTPONED(p@)")
		(sequence "TODO(t)" "|" "DONE(d!)")
		(sequence "REPORT(r)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f@)")))

(setq org-todo-keyword-faces
	  '(("TODO" . (:foreground "firebrick2" :weight bold))
		("STARTED" . (:foreground "olivedrab"' :weight bold))
		("WAITING" . (:foreground "sienna" :weight bold))
		("APPT" . (:foreground "steelblue" :weight bold))
		("DONE" . (:foreground "forestgreen" :weight bold))
		("DEFERRED" . (:foreground "forestgreen" :weight bold))
		("CANCELED" . shadow)))

(setq org-tag-alist '(("@work" . ?w)
					  ("@home" . ?h)
					  ("computer" . ?c)
					  ("project" . ?p)
					  ("reading" . ?r)
					  ("music" . ?m)
					  ("video" . ?v)))

(setq org-columns-default-format "%7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total} %38ITEM(Details) %TAGS(Context)")
(setq org-global-properties '(("Effort_All" . "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00")))
(setq org-agenda-files (list "~/org/hobby.org"
							 "~/org/study.org"
							 "~/org/work.org"))
(setq org-clock-persist t)

;; ======================================================================
;; IRC
;; ======================================================================

(custom-set-variables
 ;; auto join
 '(erc-autojoin-channels-alist
   '(("hanirc.org"
	  "#gnome"
	  "#c++"
	  "#java"
	  "#eclipse"
	  "#lisp"
	  "#perl"
	  "#python"
	  "#ruby")
	 ("freenode.net"
	  "#android"
	  "#android-dev"
	  "##gnome"
	  "#ubuntu"
	  "#lisp"
	  "#scheme"
	  "#haskell"
	  "##c++"
	  "#perl"
	  "#ruby"
	  "#eclipse"
	  "#emacs"
	  "#python"
	  "##java")))
 ;; for hanirc.org
 '(erc-server-coding-system (quote (cp949 . undecided))))

(setq erc-server "irc.hanirc.org" 
	  erc-port 6667 
	  erc-nick "jjong"
	  erc-user-full-name "The little prince"
	  erc-password nil
	  erc-prompt-for-password t)

(setq erc-server "irc.freenode.net" 
	  erc-port 6667 
	  erc-nick "jjong"
	  erc-password nil
	  erc-prompt-for-password t)

;; ======================================================================
;; Misc. Customization
;; ======================================================================
(load-library "j-dev-assist")
(load-library "j-highlight")

(if (eq system-type 'windows-nt)
 	(load-library "~/settings/emacs/windows/emacs")
  (load-library "~/settings/emacs/linux/emacs"))
