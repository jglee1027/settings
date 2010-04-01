(condition-case nil
	(require 'xcscope)
  (error nil))

(ffap-bindings)

(define-key global-map (kbd "C-c RET") 'semantic-complete-analyze-inline)
(define-key global-map (kbd "C-c c") 'compile)
(define-key global-map (kbd "C-c TAB") 'indent-relative)
(define-key global-map (kbd "C-x %") 'query-replace-regexp)

;; gdb
(add-hook 'gdb-mode-hook
		  (lambda()
			(global-set-key [f5] 'gud-step)
			(global-set-key (kbd "C-<f5>") 'gud-stepi)
			(global-set-key [f6] 'gud-next)
			(global-set-key (kbd "C-<f6>") 'gud-nexti)
			(global-set-key [f7] 'gud-finish)
			(global-set-key [f8] 'gud-cont)
			(global-set-key (kbd "M-<up>") 'gud-up)
			(global-set-key (kbd "M-<down>") 'gud-down)))

;;; =============================================================
;;; Syntax Highlighting, Spell Checking, Completion
;;; =============================================================
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; (setq font-lock-support-mode 'lazy-lock-mode)
(add-hook 'flyspell-mode-hook
   (function (lambda ()
        (define-key flyspell-mode-map "\M-\t"
   'flyspell-mode-complete))))

;; (add-hook 'text-mode-hook 'turn-on-flyspell-mode)

(defun turn-on-flyspell-mode ()
  (flyspell-mode 1))

;; Enable fly spell checking only for strings and comments in program modes.
;; (font lock mode should be enabled for use of this feature)
;(add-hook 'c-mode-common-hook 'turn-on-flyspell-mode-program)
;(add-hook 'sh-mode-hook 'turn-on-flyspell-mode-program)
;(add-hook 'makefile-mode-hook 'turn-on-flyspell-mode-program)
;(add-hook 'emacs-lisp-mode-hook 'turn-on-flyspell-mode-program)

(defun turn-on-flyspelL-mode-program ()
  (setq flyspell-generic-check-word-p 'program-mode-flyspell-verify)
  (flyspell-mode 1))

(defun program-mode-flyspell-verify ()
  (let ((prop (get-text-property (point) 'face)))
    (or (eq prop 'font-lock-string-face)
 (eq prop 'font-lock-comment-face))))

(defun flyspell-mode-complete ()
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
		 (ispell-complete-word))
		((or (eq major-mode 'latex-mode)
			 (eq major-mode 'tex-mode))
		 (or (TeX-complete-symbol)
			 (ispell-complete-word)))
		(t
		 (let ((prop (get-text-property (point) 'face)))
		   (if (or (eq prop 'font-lock-string-face)
				   (eq prop 'font-lock-comment-face))
			   (ispell-complete-word)
			 (let ((complete-func (lookup-key (current-local-map) "\M-\t")))
			   (if complete-func
				   (funcall complete-func)
				 (complete-tag))))))))

(setq Info-default-directory-list
      (append '("/usr/local/teTeX/info/") Info-default-directory-list ))

;;; =============================================================
;;; Font & Color
;;; =============================================================
;; To see available colors, run `M-x list-colors-dislay'

(set-cursor-color "khaki")
;; (set-face-foreground 'region "white")
(set-face-background 'region "mediumblue")

;; mode-line color
;; (when enable-multibyte-characters
;;  (set-face-foreground 'modeline "black")
;;  (set-face-background 'modeline "white"))
;; (unless enable-multibyte-characters
;;  (set-face-foreground 'modeline "yellow")
;;  (set-face-background 'modeline "black"))

(when window-system
  (set-background-color "#e0f0e0")
  (set-foreground-color "black")
  (set-face-font 'default (font-spec :family "Andale Mono"
									 :size 11.0))
  (set-fontset-font nil
					'korean-ksc5601
					(font-spec :family "NanumGothic_AndaleMono"
							   :registry "unicode-bmp"
							   :lang "ko"
							   :size 11.0))
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(column-number-mode t)
   '(scroll-bar-mode (quote right))
   '(tool-bar-mode nil)
   '(transient-mark-mode t))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   
   '(cscope-line-face ((((class color) (background dark)) (:foreground "yellow"))))
   '(font-lock-built-in-face ((t (:foreground "magenta"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "SkyBlue4"))))
   '(font-lock-comment-face ((t (:foreground "SkyBlue4"))))
   '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "magenta"))))
   '(font-lock-doc-face ((t (:foreground "SkyBlue4"))))
   '(font-lock-function-name-face ((t (:foreground "cyan4"))))
   '(font-lock-keyword-face ((t (:foreground "yellow4"))))
   '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
   '(font-lock-string-face ((t (:foreground "red"))))
   '(font-lock-variable-name-face ((t (:foreground "gray40"))))

   '(isearch ((t (:background "red" :foreground "black"))))
   '(isearch-lazy-highlight-face ((t (:background "gray" :foreground "black"))))

   '(jde-java-font-lock-modifier-face ((t (:foreground "yellow"))))
   '(jde-java-font-lock-operator-face ((t (:foreground "yellow"))))
   '(jde-java-font-lock-package-face ((t (:foreground "green"))))
   '(lazy-highlight ((t (:background "yellow")))))
  )
(unless window-system
  (custom-set-faces
   '(font-lock-builtin-face ((t (:foreground "magenta"))))
   '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
   '(font-lock-comment-face ((t (:foreground "blue"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "blue"))))
   '(font-lock-constant-face ((t (:foreground "green"))))
   '(font-lock-doc-face ((t (:foreground "blue"))))
   '(font-lock-function-name-face ((t (:foreground "cyan"))))
   '(font-lock-keyword-face ((t (:foreground "yellow"))))
   '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
   '(font-lock-string-face ((t (:foreground "red"))))
   '(font-lock-type-face ((t (:foreground "green"))))
   '(font-lock-variable-name-face ((t (:foreground "gray"))))

   '(isearch ((t (:background "red" :foreground "black"))))
   '(isearch-lazy-highlight-face ((t (:background "gray" :foreground "black"))))

   '(cscope-line-face ((((class color) (background dark)) (:foreground "yellow")))))
  )

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq semantic-load-turn-everything-on t)

;;; =============================================================
;;; Programming Modes
;;; =============================================================
(setq compilation-scroll-output t)

(setq auto-mode-alist (append '(("\\.mm$" . objc-mode)
								("\\.m$" . objc-mode)) auto-mode-alist))
(c-add-style
 "java"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset 0 . 0)
   (comment-color "blue" )
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


;;; =============================================================
;;; Compilation Modes
;;; =============================================================
;; To avoid cursor disappearing problem.
;; There should be better solution.

(if (and (eq system-type 'windows-nt)
         (null window-system))
    (progn
      (setq-default grep-command "grep -n -e")
      (setq-default grep-find-command
                    '("find . -type f -exec grep -n -e {} /dev/null \\;" . 32))
      (setq-default grep-find-use-xargs nil)))


;;; =============================================================
;;; Version Control
;;; =============================================================

(setq vc-make-backup-files t)

;;; =============================================================
;;; Settings for Non-standard Packages
;;; =============================================================

;;; =============================================================
;;; Crypting
;;; ftp://archive.cis.ohio-state.edu/pub/emacs-lisp/misc/crypt++.el.Z
;;; http://www.cs.washington.edu/homes/voelker/ntemacs.html
;;; =============================================================

(condition-case nil
    (require 'crypt++)
  (error nil))

;;; =============================================================
;;; Calcualtor (ftp://ftp.hanq.net/pub/gnu/calc-2.02f.tar.gz)
;;; =============================================================
;;; Commands added by calc-public-autoloads on Mon Oct  6 21:31:58 1997.
(autoload 'calc-dispatch    "calc" "Calculator Options" t)
(autoload 'full-calc     "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad    "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval     "calc" "Use Calculator from Lisp")
(autoload 'defmath     "calc" nil t t)
(autoload 'calc      "calc" "Calculator Mode" t)
(autoload 'quick-calc     "calc" "Quick Calculator" t)
(autoload 'calc-keypad     "calc" "X windows Calculator" t)
(autoload 'calc-embedded    "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region    "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle    "calc" "Grab rectangle of data" t)
(global-set-key "\e#" 'calc-dispatch)

;;; =============================================================
;;; HTML Helper Mode (http://www.santafe.edu/~nelson/tools/)
;;; =============================================================

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq auto-mode-alist (append '(("\\.html$" . html-helper-mode)
;;                                 ("\\.jsp$" . html-helper-mode))
;;                               auto-mode-alist))
(add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
(add-hook 'html-helper-mode-hook '(lambda () (font-lock-mode 1)))
(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)

;;; =============================================================
;;; AUC TeX - Integrated environment for editing LaTeX files.
;;; (ftp://ftp.dante.de/tex-archive/support/auctex)
;;; teTeX, MikTeX (http://free.kaist.ac.kr/ChoF/)
;;; =============================================================

(condition-case nil
    (require 'tex-site)
  (error nil))

(add-hook
 'TeX-mode-hook
 (function
  (lambda ()
    (define-key TeX-mode-map "\C-x`" 'TeX-next-error)
    (define-key LaTeX-mode-map "\C-x`" 'TeX-next-error))))

(if (eq system-type 'windows-nt)
    (setq TeX-view-style '(("^a5$" "yap %d -paper a5")
                           ("^landscape$" "yap %d -paper a4r -s 4")
                           ("." "yap %d"))))


;;; =============================================================
;;; JDE - Java Development Environment (http://sunsite.auc.dk/jde/)
;;; =============================================================
;;; Copy `/usr/local/share/emacs/20.3/site-lisp/jde-2.1.3/jtags*' to `/usr/local/bin/'

(condition-case nil
    (require 'jde)
  (error nil))

(if (eq system-type 'windows-nt)
    (setq jde-db-source-directories '("C:/jdk1.2/src/"))
  (setq jde-db-source-directories '("/usr/local/jdk1.2/src")))


;;; =============================================================
;;; Telnet (http://www.watchit.com/~iganza/jtelnet.html) for Windows 95/NT
;;; =============================================================
;; Install JDK from http://java.sun.com/
;; Change followings properly for your system configuration

(if (eq system-type 'windows-nt)
    (progn
      (setq jtelnet-java-path "java")
      ;; for JDK 1.2.x
      (setq jtelnet-class-arg "-cp")
      (setq jtelnet-class-path
			(concat
			 (apply 'concat (mapcar
							 (lambda (x) (if (= x ?/) "\\" (char-to-string x)))
							 data-directory))
			 "..\\site-lisp\\jtelnet"))
      ;; for JDK 1.1.x
      ;; (setq jtelnet-class-arg "-classpath")
      ;; (setq jtelnet-class-path
      ;;      (concat "d:\\jdk1.1\\lib\\classes.zip;"
      ;;              (apply 'concat (mapcar
      ;;                              (lambda (x) (if (= x ?/) "\\" (char-to-string x)))
      ;;                              data-directory))
      ;;              "..\\site-lisp\\jtelnet")))
      (condition-case nil
		  (require 'jtelnet)
		(error nil))))
 

;;; ================================================================================
;;; Talk (ftp://archive.cis.ohio-state.edu/pub/emacs-lisp/packages/etalk-0.8.tar.gz)
;;; ================================================================================
;;; Need to build and install "etalk" executable in Emacs bin directory.

(autoload 'etalk "etalk"
  "Talk to anyone on any machine through BSD talk protocol." t)
(autoload 'etalk-batch "etalk"
  "Talk to anyone on any machine through BSD talk protocol from command line." t)
(autoload 'etalk-mail "etalk"
  "Talk to anyone on any machine through mail as connector." t)
(autoload 'finger "finger"
  "Run finger on a host." t)



;;; =============================================================
;;; Private Settting and Customization
;;; =============================================================
(defvar default-encoding
  (let (lang)
  (setq lang (getenv "LANG"))
  (cond ((not (equal (string-match "UTF-8" lang) nil))
		 'utf-8)
		((not (equal (string-match "EUC-KR" lang) nil))
		 'euc-kr)
		(t
		 'utf-8))))

(when enable-multibyte-characters
  (set-language-environment "Korean")
  
  (setq-default file-name-coding-system default-encoding)
  (setq default-korean-keyboard "3")
  ;; (setq input-method-verbose-flag nil
  ;;       input-method-highlight-flag nil)
  (prefer-coding-system default-encoding)
  (set-default-coding-systems default-encoding)

  (set-keyboard-coding-system default-encoding)
  (set-terminal-coding-system default-encoding)
  (define-key encoded-kbd-mode-map [27] nil)
  
  ;; (set-selection-coding-system 'compound-text-with-extensions)
  (set-selection-coding-system
   (cond ((eq system-type 'windows-nt) 'cp949-dos)
		 (t 'utf-8)))
  
  (unless window-system
	(menu-bar-mode -1))
  
  ;; Hangul Mail setting
  (setq-default sendmail-coding-system default-encoding))

(unless (or enable-multibyte-characters window-system)
  (standard-display-european t)
  (set-input-mode (car (current-input-mode))
                  (nth 1 (current-input-mode))
                  0))

;; sealover's setup (by Learning GNU Emacs)
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq-default transient-mark-mode t)
(show-paren-mode 1)

;; (enable-flow-control)
(global-unset-key "\e\e")
(global-unset-key "\C-x\C-u")
;; (setq suggest-key-bindings nil)

;; shell environment
(setq-default shell-cd-regexp nil)
(setq-default shell-pushd-regexp nil)
(setq-default shell-popd-regexp nil)
;; (setq explicit-shell-file-name "/usr/local/bin/bash")
;; secure on shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; default tab
(setq c-basic-offset 4)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq column-number-mode t)

(setq truncate-partial-width-windows nil)

;;; ============================================================
;;; IRC
;;; ============================================================

(custom-set-variables
 ;; auto join
 '(erc-autojoin-channels-alist
   (quote (("hanirc.org" "#gnome" "#c++" "#java" "#eclipse" "#lisp" "#perl" "#python" "#ruby") 
		   ("freenode.net" "##gnome" "#ubuntu" "#lisp" "#scheme" "#haskell" "##c++" "#perl" "#ruby" "#eclipse" "#emacs" "#python" "##java"))))
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

;;; ============================================================
;;; org-mode
;;; ============================================================

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

;;; =============================================================
;;; Misc.
;;; =============================================================
(autoload 'which "which" nil t)