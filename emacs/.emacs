;; ======================================================================
;; General setting
;; ======================================================================
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
(which-function-mode)
(windmove-default-keybindings)
(condition-case nil
	(progn
	  (require 'auto-complete)
	  (global-auto-complete-mode t))
  (error nil))

(setq semantic-load-turn-everything-on t)
(setq vc-make-backup-files t)

(when window-system
  (tool-bar-mode nil))
(unless window-system
  (menu-bar-mode nil))

;; Language setting
(defvar default-encoding
  (let (lang)
	(setq lang (getenv "LANG"))
	(cond ((not (equal (string-match "UTF-8" lang) nil))
		   'utf-8)
		  ((not (equal (string-match "EUC-KR" lang) nil))
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
  (set-terminal-coding-system default-encoding)
  (define-key encoded-kbd-mode-map [27] nil)
  
  (set-selection-coding-system
   (cond ((eq system-type 'windows-nt) 'cp949-dos)
		 (t 'utf-8)))
  
  ;; Hangul Mail setting
  (setq-default sendmail-coding-system default-encoding))

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

;; ======================================================================
;; Key definition
;; ======================================================================
(global-set-key [C-kanji] 'set-mark-command)
(global-set-key [?\S- ] 'toggle-input-method)
(define-key global-map (kbd "C-x RET s") 'decode-coding-region)
(define-key global-map (kbd "C-c C-s a") 'semantic-complete-analyze-inline)
(define-key global-map (kbd "C-c RET") 'semantic-ia-complete-symbol-menu)
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

;; gdb
(add-hook 'gdb-mode-hook
		  (lambda()
			(global-set-key [f5] 'gud-step)
			(global-set-key (kbd "C-<f5>") 'gud-stepi)
			(global-set-key [f6] 'gud-next)
			(global-set-key (kbd "C-<f6>") 'gud-nexti)
			(global-set-key [f7] 'gud-finish)
			(global-set-key [f8] 'gud-cont)
			(global-set-key [f12] 'gdb-many-windows)
			(global-set-key (kbd "M-<up>") 'gud-up)
			(global-set-key (kbd "M-<down>") 'gud-down)))

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

(setq auto-mode-alist (append '(("\\.cs$" . java-mode)
								("\\.h$" . c++-mode)
								("\\.m$" . objc-mode)
								("\\.mm$" . objc-mode)
								("\\.rb$" . ruby-mode)
								("\\.cflow$" . cflow-mode))
							  auto-mode-alist))

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

;; ======================================================================
;; Cscope
;; ======================================================================
(condition-case nil
    (require 'xcscope)
  (error nil))

(add-hook 'java-mode-hook (function cscope:hook))
(add-hook 'asm-mode-hook (function cscope:hook))
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
;; Org-mode
;; ======================================================================
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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
   (quote (("hanirc.org"
			"#gnome"
			"#c++"
			"#java"
			"#eclipse"
			"#lisp"
			"#perl"
			"#python"
			"#ruby")
		   ("freenode.net"
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
			"##java"))))
 ;; for hanirc.org
 '(erc-server-coding-system (quote (cp949 . undecided))))

(setq erc-server "irc.freenode.net" 
	  erc-port 6667 
	  erc-nick "jjong"
	  erc-password nil
	  erc-prompt-for-password t)

(setq erc-server "irc.hanirc.org" 
	  erc-port 6667 
	  erc-nick "jjong"
	  erc-user-full-name "The little prince"
	  erc-password nil
	  erc-prompt-for-password t)

;; ======================================================================
;; Misc. Customization
;; ======================================================================
(load-library "j-util")
(load-library "j-highlight")

(if (eq system-type 'windows-nt)
 	(load-library "~/settings/emacs/windows/emacs")
  (load-library "~/settings/emacs/linux/emacs"))
