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
 '(default ((t (:inherit nil
			    :stipple nil
				:background "#102050"
				:foreground "white"
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
 '(compilation-info ((t (:foreground "DodgerBlue"))))
 '(compilation-warning ((t (:foreground "Orange"))))
 '(cscope-line-face ((nil)))
 '(cscope-line-number-face ((nil)))
 '(cursor ((t (:background "yellow" :foreground "black"))))
 '(ecb-default-highlight-face ((t (:inherit highlight))))
 '(ecb-type-tag-class-face ((t (:foreground "green"))))
 '(ecb-type-tag-enum-face ((t (:foreground "green"))))
 '(ecb-type-tag-group-face ((t (:foreground "green"))))
 '(ecb-type-tag-interface-face ((t (:foreground "green"))))
 '(ecb-type-tag-struct-face ((t (:foreground "green"))))
 '(ecb-type-tag-typedef-face ((t (:foreground "green"))))
 '(ecb-type-tag-union-face ((t (:foreground "green"))))
 '(font-lock-builtin-face ((t (:foreground "violet"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-comment-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-constant-face ((t (:foreground "green"))))
 '(font-lock-doc-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "yellow"))))
 '(font-lock-preprocessor-face ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "red"))))
 '(font-lock-type-face ((t (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "gray70"))))
 '(fringe ((nil)))
 '(highlight ((t (:background "SkyBlue" :foreground "black"))))
 '(isearch ((t (:background "red" :foreground "black"))))
 '(lazy-highlight ((t (:background "DarkGray" :foreground "black"))))
 '(mode-line ((t (:background "gray" :foreground "black"))))
 '(mouse ((t nil))))

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
