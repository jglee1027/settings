;; ===========================================================================
;; General setting
;; ===========================================================================
(setq default-tab-width 4)
(setq tab-stop-list
	  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76  80 84 88 92 96 100 104 108 112 116 120))
(setq scalable-fonts-allowed t)
(ffap-bindings)
(setq compilation-scroll-output t)

;; ===========================================================================
;; Define key
;; ===========================================================================
(global-set-key [C-kanji] 'set-mark-command)
(define-key global-map (kbd "C-x RET s") 'decode-coding-region)
(define-key global-map (kbd "C-c C-s a") 'semantic-complete-analyze-inline)
(define-key global-map (kbd "C-c RET") 'semantic-ia-complete-symbol-menu)
(define-key global-map (kbd "C-c c") 'compile)
(define-key global-map (kbd "C-c TAB") 'indent-relative)
(define-key global-map (kbd "C-x p") 'previous-buffer)
(define-key global-map (kbd "C-x n") 'next-buffer)

;; ===========================================================================
;; Korean language support
;; ===========================================================================
(set-language-environment "Korean")
(icomplete-mode)
(partial-completion-mode 1)
(column-number-mode t)

;; ===========================================================================
;; Mode line and minibuffer
;; ===========================================================================
(setq display-time-string-forms
	  '((if
			(and
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

;; ===========================================================================
;; Programming modes
;; ===========================================================================
(setq auto-mode-alist (append '(("\\.cs$" . java-mode))
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

;; ===========================================================================
;; Ruby mode
;; ===========================================================================
(condition-case nil
	(progn
	  (require 'ruby-mode)
	  (require 'rubydb)
	  (require 'inf-ruby)
	  (require 'ruby-electric))
  (error nil))

(setq auto-mode-alist (cons '("\.rb$" . ruby-mode) auto-mode-alist))

;; ===========================================================================
;; Cscope
;; ===========================================================================
(condition-case nil
    (require 'xcscope)
  (error nil))
										;(setq cscope-do-not-update-database 't)
(add-hook 'java-mode-hook (function cscope:hook))
(add-hook 'asm-mode-hook (function cscope:hook))
(autoload 'cflow-mode "cflow-mode")
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.cflow$" . cflow-mode))))

;; Source Navigator
(condition-case nil
    (require 'sn)
  (error nil))

;; ===========================================================================
;; CEDET and ECB
;; ===========================================================================
(condition-case nil
	(progn
	  (require 'cedet)
	  (require 'ecb))
  (error nil))

;; ===========================================================================
;; Custom variables and faces
;; ===========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(erc-autojoin-channels-alist (quote (("hanirc.org"
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
 '(erc-server-coding-system (quote (cp949 . undecided)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil
						 :stipple nil
						 :background "#e0f0e0"
						 :foreground "Black"
						 :inverse-video nil
						 :box nil
						 :strike-through nil
						 :overline nil
						 :underline nil
						 :slant normal
						 :weight normal
						 :height 98
						 :width normal
						 :foundry "outline"
						 :family "³ª´®°íµñ_AndaleMono"))))
 '(cscope-line-face ((((class color) (background dark)) (:foreground "yellow"))))
 '(font-lock-built-in-face ((t (:foreground "magenta"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "blue4"))))
 '(font-lock-comment-face ((t (:foreground "blue4"))))
 '(font-lock-doc-face ((t (:foreground "blue4"))))
 '(font-lock-function-name-face ((t (:foreground "cyan4"))))
 '(font-lock-keyword-face ((t (:foreground "yellow4"))))
 '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "red4"))))
 '(font-lock-variable-name-face ((t (:foreground "gray50"))))
 '(isearch ((t (:background "red"))))
 '(lazy-highlight ((t (:background "gray")))))

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
