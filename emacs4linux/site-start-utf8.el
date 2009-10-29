(require 'cl)
(require 'xcscope)
(require 'ecb)

(when enable-multibyte-characters
  (set-language-environment "Korean")
 
(setq-default file-name-coding-system 'utf-8)

;; the following setting is unnecessary from 20.5 >

;;(when (string-match "^3" (or (getenv "HANGUL_KEYBOARD_TYPE") ""))
 
(setq default-korean-keyboard "")
(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)
 
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;;(setq default-process-coding-system '(utf-8 . utf-8))
(when window-system
  (global-set-key "\C-\\" 'undefined))
  (add-hook 'quail-inactivate-hook 'delete-quail-completions)
(defun delete-quail-completions ()
  (when (get-buffer "*Quail Completions*")
    (kill-buffer "*Quail Completions*")))

(unless window-system
  (menu-bar-mode -1)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (define-key encoded-kbd-mode-map [27] nil))

(set-selection-coding-system 'ctext)

;; Hangul Mail setting
(setq-default sendmail-coding-system 'utf-8)

;; turn off C-h during input
(eval-after-load "quail"
  '(progn
    (define-key quail-translation-keymap "\C-h" 'quail-delete-last-char)
    (define-key quail-translation-keymap (kbd "C-SPC") 'set-mark-command)
    (define-key quail-translation-keymap "\C-?" 'quail-translation-help)))
(define-key global-map (kbd "C-x RET s") 'decode-coding-region))
(define-key global-map (kbd "C-c RET") 'semantic-complete-analyze-inline)
(define-key global-map (kbd "C-c c") 'compile)
(define-key global-map (kbd "C-c TAB") 'indent-relative)
(define-key global-map (kbd "C-x %") 'query-replace-regexp)
(define-key global-map (kbd "C-x p") 'previous-buffer)
(define-key global-map (kbd "C-x n") 'next-buffer)
;; gdb
(define-key global-map (kbd "<f5>") 'gud-step)
(define-key global-map (kbd "<f6>") 'gud-next)
(define-key global-map (kbd "<f7>") 'gud-finish)

(setq compilation-scroll-output t)

;;; =============================================================
;;; Syntax Highlighting, Spell Checking, Completion
;;; =============================================================

(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
;(setq font-lock-support-mode 'lazy-lock-mode)
(add-hook 'flyspell-mode-hook
   (function (lambda ()
        (define-key flyspell-mode-map "\M-\t"
   'flyspell-mode-complete))))

;(add-hook 'text-mode-hook 'turn-on-flyspell-mode)

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
;;; Text Mode
;;; =============================================================

(setq default-major-mode (quote text-mode))

;(add-hook 'text-mode-hook
;          (function (lambda ()
;                      (setq fill-column 70)
;                      (turn-on-auto-fill))))

;;; =============================================================
;;; Programming Modes
;;; =============================================================

(c-add-style
 "java"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset 0 . 0)
   ( comment-color "blue" )
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
; To avoid cursor disappearing problem.
; There should be better solution.

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
;;; End of Calc autoloads.


;;; =============================================================
;;; HTML Helper Mode (http://www.santafe.edu/~nelson/tools/)
;;; =============================================================

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (append '(("\\.html$" . html-helper-mode)
;                                ("\\.jsp$" . html-helper-mode))
;                              auto-mode-alist))
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
      ;(setq jtelnet-class-arg "-classpath")
      ;(setq jtelnet-class-path
      ;      (concat "d:\\jdk1.1\\lib\\classes.zip;"
      ;              (apply 'concat (mapcar
      ;                              (lambda (x) (if (= x ?/) "\\" (char-to-string x)))
      ;                              data-directory))
      ;              "..\\site-lisp\\jtelnet")))
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
;;; Misc.
;;; =============================================================

(autoload 'which "which" nil t)


;;; =============================================================
;;; Private Settting and Customization
;;; =============================================================

;; To see available colors, run `M-x list-colors-dislay'


 
(require 'cl)
(when enable-multibyte-characters
  (set-language-environment "Korean")
 
  (setq-default file-name-coding-system 'utf-8)
  (setq default-korean-keyboard "3")
  ;; (setq input-method-verbose-flag nil
  ;;       input-method-highlight-flag nil)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (define-key encoded-kbd-mode-map [27] nil)
 
;  (set-selection-coding-system 'compound-text-with-extensions)
(set-selection-coding-system
 (cond ((eq system-type 'windows-nt) 'utf-8-dos)
       (t 'utf-8)))

 
  ;; Hangul Mail setting
  (setq-default sendmail-coding-system 'utf-8))
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

;;(enable-flow-control)
(global-unset-key "\e\e")
(global-unset-key "\C-x\C-u")
(define-key global-map "\C-x?" 'help-command)
(define-key global-map "\C-h" 'backward-char)
;;(setq suggest-key-bindings nil)
(when window-system 
	(set-background-color "#e0f0e0")
	(set-foreground-color "black")
	(set-face-font 'default (font-spec :family "Andale Mono" :size 12.0))
	(set-fontset-font nil 'korean-ksc5601 (font-spec :family "NanumGothic_AndaleMono" :registry "unicode-bmp" :lang "ko" :size 12.0)))

(set-cursor-color "khaki")
;;(set-face-foreground 'region "white")
(set-face-background 'region "mediumblue")
;;(global-font-lock-mode 1 t)

;; mode-line color
;; (when enable-multibyte-characters
;;  (set-face-foreground 'modeline "black")
;;  (set-face-background 'modeline "white"))
;; (unless enable-multibyte-characters
;;  (set-face-foreground 'modeline "yellow")
;;  (set-face-background 'modeline "black"))
;;shell environment
(setq-default shell-cd-regexp nil)
(setq-default shell-pushd-regexp nil)
(setq-default shell-popd-regexp nil)
;(setq explicit-shell-file-name "/usr/local/bin/bash")
;; secure on shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(setq column-number-mode t) 

;; c-mode
(add-hook 'c-mode-hook '(lambda() (c-set-style "gnu")))

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq truncate-partial-width-windows nil)

;; color setting
(when window-system
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
	 '(isearch ((t (:background "red"))))
	 '(jde-java-font-lock-modifier-face ((((class color) (background dark)) (:foreground "yellow"))))
	 '(jde-java-font-lock-operator-face ((((class color)) (:foreground "yellow"))))
	 '(jde-java-font-lock-package-face ((((class color) (background dark)) (:foreground "green"))))
	 '(lazy-highlight ((t (:background "yellow")))))
)
(unless window-system
	(custom-set-faces
		'(font-lock-built-in-face ((t (:foreground "magenta"))))
		'(font-lock-preprocessor-face ((t (:foreground "magenta"))))
		'(font-lock-comment-face ((t (:foreground "blue"))))
		'(font-lock-comment-delimiter-face ((t (:foreground "blue"))))
		'(font-lock-string-face ((t (:foreground "red"))))
		'(font-lock-doc-face ((t (:foreground "blue"))))
		'(font-lock-keyword-face ((t (:foreground "yellow"))))
		'(font-lock-function-name-face ((t (:foreground "cyan"))))
		'(font-lock-variable-name-face ((t (:foreground "gray"))))

		'(isearch ((t (:background "red"))))
		'(isearch-lazy-highlight-face ((t (:background "yellow"))))

		'(cscope-line-face ((((class color) (background dark)) (:foreground "yellow")))))
)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq semantic-load-turn-everything-on t)
(require 'semantic-load)

;; ============================================================
;; IRC
;; ============================================================

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

;; eof
