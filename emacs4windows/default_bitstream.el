;; -*- coding: korean-iso-8bit; -*-
;;; "~/.emacs" and "site-lisp/default.el" file for Emacs Korean users
;;  mainly for Emacs 21.3+, 22 on Windows, Unix.
;;  Author: Deogtae Kim <deogtae@yahoo.co.kr>, 2006/3/4
 
;; Following is for inhibiting loading "default.el" to have your "~/.emacs" file
;; override "default.el" completely.
(setq inhibit-default-init t)
(setq default-tab-width 4)
(require 'xcscope)
(setq scalable-fonts-allowed t)

;;; =============================================================
;;; Korean Language Environment
;;; =============================================================

;; General korean language environment
(set-language-environment "Korean")   

;; Emacs 22 automatically set following coding systems.
(if (<= emacs-major-version 21)
    (progn
      (set-selection-coding-system 
       (cond ((eq system-type 'windows-nt) 'euc-kr-dos)
             (t 'euc-kr)))
      (set-keyboard-coding-system 'euc-kr)
      (if (eq system-type 'windows-nt)
          (set-w32-system-coding-system 'korean-iso-8bit))))

;;; =============================================================
;;; Fonts and Frame for Unix X11
;;; =============================================================

(if (eq window-system 'x)
    (progn
      ;; For clean Ascii and its bold font.
      ;;  No need to install additional fonts.
      ;;  But, this does not show ISO-8859-1 character sets.
      ;;  Seems to be a bug of Emacs 20.2 where ISO-8859-1 charset font
      ;;  setting is overriden by ASCII charset font setting
      ;;  Bold font is shown as plain font.
      (create-fontset-from-fontset-spec
       "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-clean16,
       ascii:-schumacher-clean-medium-r-normal-*-16-*-iso8859-1,
       latin-iso8859-1:-sony-fixed-medium-r-normal-*-16-*-iso8859-1,
       korean-ksc5601:-*-mincho-medium-r-normal-*-16-*-ksc5601*-*,
       ;korean-ksc5601:-sun-gothic-medium-r-normal-*-16-160-*-ksc5601*-*,
       chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
       chinese-cns11643-1:-*-medium-r-normal-*-16-*-cns11643*-1,
       chinese-cns11643-2:-*-medium-r-normal-*-16-*-cns11643*-2,
       chinese-cns11643-3:-*-medium-r-normal-*-16-*-cns11643*-3,
       chinese-cns11643-4:-*-medium-r-normal-*-16-*-cns11643*-4,
       chinese-cns11643-5:-*-medium-r-normal-*-16-*-cns11643*-5,
       chinese-cns11643-6:-*-medium-r-normal-*-16-*-cns11643*-6,
       chinese-cns11643-7:-*-medium-r-normal-*-16-*-cns11643*-7"
       t)
   
      ;; Good font set with size 16 for multilingual environment if you installed
      ;;  all fonts in `ftp://ftp.gnu.org/pub/gnu/intlfonts'
      (create-fontset-from-fontset-spec
       "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-etl16,
       ascii:-etl-fixed-medium-r-normal-*-16-*-*-*-*-*-iso8859-1,
       lao:-*-fixed-medium-r-normal-*-16-*-mulelao-1,
       tibetan:-*-fixed-medium-r-normal-*-16-*-muletibetan-0,
       tibetan-1-column:-*-medium-r-normal-*-16-*-muletibetan-1,
       thai-tis620:-*-medium-r-normal-*-16-*-tis620.2529-*,
       korean-ksc5601:-*-mincho-medium-r-normal-*-16-*-ksc5601*-*,
       latin-jisx0201:-*-fixed-medium-r-normal-*-16-*-jisx0201*-*,
       katakana-jisx0201:-*-fixed-medium-r-normal-*-16-*-jisx0201*-*,
       chinese-big5-1:-*-fixed-medium-r-normal-*-16-*-big5.eten-0,
       chinese-big5-2:-*-fixed-medium-r-normal-*-16-*-big5.eten-0,
       chinese-gb2312:-*-medium-r-normal-*-16-*-gb2312*-*,
       chinese-cns11643-1:-*-medium-r-normal-*-16-*-cns11643*-1,
       chinese-cns11643-2:-*-medium-r-normal-*-16-*-cns11643*-2,
       chinese-cns11643-3:-*-medium-r-normal-*-16-*-cns11643*-3,
       chinese-cns11643-4:-*-medium-r-normal-*-16-*-cns11643*-4,
       chinese-cns11643-5:-*-medium-r-normal-*-16-*-cns11643*-5,
       chinese-cns11643-6:-*-medium-r-normal-*-16-*-cns11643*-6,
       chinese-cns11643-7:-*-medium-r-normal-*-16-*-cns11643*-7" t)

      ;; Good font set with size 24 for multilingual environment 
      ;; and esp. for Chinese if you installed
      ;; all fonts in `ftp://ftp.gnu.org/pub/gnu/intlfonts'
      (create-fontset-from-fontset-spec
       "-*-fixed-medium-r-normal-*-24-*-*-*-*-*-fontset-etl24,
      ascii:-etl-fixed-medium-r-normal-*-16-*-*-*-*-*-iso8859-1,
      lao:-*-fixed-medium-r-normal-*-24-*-mulelao-1,
      tibetan:-*-fixed-medium-r-normal-*-24-*-muletibetan-0,
      tibetan-1-column:-*-medium-r-normal-*-24-*-muletibetan-1,
      thai-tis620:-*-medium-r-normal-*-24-*-tis620.2529-*,
      korean-ksc5601:-*-mincho-medium-r-normal-*-24-*-ksc5601*-*,
      latin-jisx0201:-*-fixed-medium-r-normal-*-24-*-jisx0201*-*,
      katakana-jisx0201:-*-fixed-medium-r-normal-*-24-*-jisx0201*-*,
      chinese-big5-1:-*-fixed-medium-r-normal-*-24-*-big5.eten-0,
      chinese-big5-2:-*-fixed-medium-r-normal-*-24-*-big5.eten-0,
      chinese-gb2312:-*-medium-r-normal-*-24-*-gb2312*-*,
      chinese-cns11643-1:-*-medium-r-normal-*-24-*-cns11643*-1,
      chinese-cns11643-2:-*-medium-r-normal-*-24-*-cns11643*-2,
      chinese-cns11643-3:-*-medium-r-normal-*-24-*-cns11643*-3,
      chinese-cns11643-4:-*-medium-r-normal-*-24-*-cns11643*-4,
      chinese-cns11643-5:-*-medium-r-normal-*-24-*-cns11643*-5,
      chinese-cns11643-6:-*-medium-r-normal-*-24-*-cns11643*-6,
      chinese-cns11643-7:-*-medium-r-normal-*-24-*-cns11643*-7" t)

      ;; (setq initial-frame-alist '((top . 10) (left . 30)))
      (setq default-frame-alist
            (append '(
                      ;; Choose only one for your default font set.
                      ;;(font . "fontset-standard")
                      (font . "fontset-clean16")
                      ;;(font . "fontset-etl16")
                      ;;(font . "fontset-etl24")
                      (width . 120)
                      (height . 50)
                      (background-color . "Black")
                      (foreground-color . "PaleGreen")
                      (vertical-scroll-bars . right))
                    default-frame-alist))))

;;; =============================================================
;;; Fonts and Frame for Windows 95/NT
;;; For multilingual display, get fonts from 
;;; http://www.microsoft.com,
;;; http://babel.uoregon.edu/yamada/guides.html
;;; =============================================================

;; font spec format: -foundry-font family-weight-slant-?-?-size(pixels)-size(points)-hres-vres-spacing-avg width-charset

;; To see list of fonts in your Windows 95/NT,
;  (prin1-to-string (w32-select-font)) or
;  (prin1-to-string (x-list-fonts "*")) 
;; Korean fonts: Gulim, GulimChe, Batang, BatangChe, Dotum, DotumChe, Gungsuh, GungsuhChe
;; In Emacs 22, you should use Korean font names for korean fonts like "굴림체" for things to go well,
;; and had better turn off font smoothing on Windows desktop setting to avoid ugly text rendering.

(setq mac-allow-anti-aliasing nil)
;; For Emacs 21 font set settings
(condition-case nil
    (if (eq window-system 'w32)
        (progn
          ;; slightly ugly Korean font for exact ratio of widths of English and CJK fonts.
          (create-fontset-from-fontset-spec
           "-*-Bitstream Vera Sans Mono-normal-r-*-*-12-*-*-*-c-*-fontset-mscourier12c,
 	 ascii:-*-Bitstream Vera Sans Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-1,
         korean-ksc5601:-*-굴림체-normal-r-*-*-13-*-*-*-c-*-ksc5601.1987*-*,
 	 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-13-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-13-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0212:-*-MS Gothic-normal-r-*-*-13-*-*-*-c-*-jisx0212-sjis,
  	 chinese-gb2312:-*-SimSun-normal-r-*-*-13-*-*-*-c-*-gb2312-*,
  	 chinese-big5-1:-*-MingLiU-normal-r-*-*-13-*-*-*-c-*-big5-*,
 	 chinese-big5-2:-*-MingLiU-normal-r-*-*-13-*-*-*-c-*-big5-*"
           t)

          (create-fontset-from-fontset-spec
           "-*-Bitstream Vera Sans Mono-normal-r-*-*-12-*-*-*-c-*-fontset-mscourier12,
 	 ascii:-*-Bitstream Vera Sans Mono-normal-r-*-*-12-*-*-*-c-*-iso8859-1,
	 korean-ksc5601:-*-굴림체-normal-r-*-*-12-*-*-*-c-*-ksc5601.1987*-*,
 	 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0212:-*-MS Gothic-normal-r-*-*-12-*-*-*-c-*-jisx0212-sjis,
  	 chinese-gb2312:-*-SimSun-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
  	 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 	 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*"
           t)

          (create-fontset-from-fontset-spec
           "-*-Bitstream Vera Sans Mono-normal-r-*-*-16-*-*-*-c-*-fontset-mscourier16c,
 	 ascii:-*-Bitstream Vera Sans Mono-normal-r-*-*-16-*-*-*-c-*-iso8859-1,
	 korean-ksc5601:-*-굴림체-normal-r-*-*-20-*-*-*-c-*-ksc5601.1987*-*,
 	 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-20-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-20-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0212:-*-MS Gothic-normal-r-*-*-20-*-*-*-c-*-jisx0212-sjis,
  	 chinese-gb2312:-*-SimSun-normal-r-*-*-20-*-*-*-c-*-gb2312-*,
  	 chinese-big5-1:-*-MingLiU-normal-r-*-*-20-*-*-*-c-*-big5-*,
 	 chinese-big5-2:-*-MingLiU-normal-r-*-*-20-*-*-*-c-*-big5-*"
           t)

          (create-fontset-from-fontset-spec
           "-*-Bitstream Vera Sans Mono-normal-r-*-*-16-*-*-*-c-*-fontset-mscourier16,
 	 ascii:-*-Bitstream Vera Sans Mono-normal-r-*-*-16-*-*-*-c-*-iso8859-1,
	 korean-ksc5601:-*-굴림체-normal-r-*-*-16-*-*-*-c-*-ksc5601.1987*-*,
 	 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-16-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-16-*-*-*-c-*-jisx0208-sjis,
 	 japanese-jisx0212:-*-MS Gothic-normal-r-*-*-16-*-*-*-c-*-jisx0212-sjis,
  	 chinese-gb2312:-*-SimSun-normal-r-*-*-16-*-*-*-c-*-gb2312-*,
  	 chinese-big5-1:-*-MingLiU-normal-r-* font set settings-*-16-*-*-*-c-*-big5-*,
 	 chinese-big5-2:-*-MingLiU-normal-r-*-*-16-*-*-*-c-*-big5-*"
           t)

          ;;(setq w32-enable-italics t)
          ;;(setq w32-enable-italics nil)

          (setq w32-use-w32-font-dialog nil)

          ;;(setq w32-enable-unicode-output nil)

          (setq initial-frame-alist '((top . 10) (left . 30)))
          (setq default-frame-alist
                (append '(
                          ;; Choose only one for your default font set.
                          ;;(font . "-*-Bitstream Vera Sans Mono-normal-r-*-*-13-*-*-*-c-*-fontset-standard")
                          ;;(font . "-*-Bitstream Vera Sans Mono-normal-r-*-*-16-*-*-*-c-*-fontset-most")
                          ;;(font . "fontset-mscourier12c")
                          (font . "fontset-mscourier12")
                          ;;(font . "fontset-mscourier16c")
                          ;;(font . "fontset-mscourier16")
                          (width . 120)
                          (height . 50)
                          (background-color . "Black")
                          (foreground-color . "PaleGreen")
                          (vertical-scroll-bars . right))
                        default-frame-alist))))
  (error nil))



;;; =============================================================
;;; Keyboard and Input Method
;;; =============================================================

;; C-SPC or C-\ for toggling input method.
;; F9 for switching with hanja input, C-F9 for switching with symbol input
;;    in Korean input method.
;; C-U C-SPC or C-U C-\ for other input method.

;; If your default keyboard type is 3 bulsik, uncomment following line.
; (setq default-korean-keyboard "3")

;; Selection of major korean input method determines Korean keyboard type.
;; Verbose mode of hangul input method is needless for most Korean users.
(add-hook
 'input-method-activate-hook
 (function (lambda ()
             (cond ((string= current-input-method "korean-hangul")
                    (setq default-korean-keyboard ""
                          input-method-verbose-flag nil
                          input-method-highlight-flag nil))
                   ((string= current-input-method "korean-hangul3")
                    (setq default-korean-keyboard "3"
                          input-method-verbose-flag nil
                          input-method-highlight-flag nil))))))
(add-hook
 'input-method-inactivate-hook
 (function (lambda ()
             (if (or (string= current-input-method "korean-hangul")
                     (string= current-input-method "korean-hangul3"))
                 (setq input-method-verbose-flag 'default
                       input-method-highlight-flag t)))))

;; In Unix terminal mode, make Backspace and Control-h key work as real backspace key.
;; (In any Emacs, F1 key work as an help key, too.)
(if (and (not (eq system-type 'windows-nt)) (null window-system))
    (keyboard-translate ?\C-h ?\C-?))


;;; =============================================================
;;; Mode Line and Minibuffer Settings
;;; =============================================================

(icomplete-mode)
(partial-completion-mode 1)
(column-number-mode 1)

;; For bug fix of localized date output and better display for Korean
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


;;; =============================================================
;;; Misc. Setting
;;; =============================================================

;(server-start)
(setq scroll-step 1)
(global-unset-key "\M-g")
(global-set-key "\M-g\M-g" 'goto-line)
(setq query-replace-highlight t)
(show-paren-mode)
(transient-mark-mode 1)
(auto-compression-mode 1)  ; For auto compression and decompression
(setq auto-mode-alist       
      (cons '("\\.\\(ear\\|war\\)\\'" . archive-mode) auto-mode-alist))
(global-unset-key "\C-z")

(autoload 'which "which" nil t)


;;; =============================================================
;;; Command Shell
;;; =============================================================
;; set SHELL environment variable correctly for emacs in Windows 95/NT
;;   even when user already set it.

;; For preventing Control-M printing
;;(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; For use of command.com in Windows 95/NT Emacs.
;; You had better copy c:\autoexec.bat to ~/.emacs_cmdproxy
;; Doesn't properly work in Emacs 20.3, 20.4 beta!!
;; (if (eq system-type 'windows-nt)
;;     (progn
;;       (setenv "SHELL"
;; 	      (concat
;; 	       (apply 'concat
;; 		      (mapcar
;; 		       (lambda (x) (if (= x ?/) "\\" (char-to-string x)))
;; 		       data-directory))
;; 	       "..\\bin\\cmdproxy"))))

;; For use of bash in Windows 95/NT Emacs
;; (http://sourceware.cygnus.com/cygwin/)
; (if (eq system-type 'windows-nt)
;     (progn
;       (setq shell-file-name "bash.exe")
;       (setq explicit-shell-file-name shell-file-name)
;       (setenv "SHELL" shell-file-name)

;       (defun my-shell-setup ()
; 	(setq comint-scroll-show-maximum-output 'this)
; 	(setq comint-completion-addsuffix t)
; 	(setq comint-process-echoes t)
; 	(setq comint-eol-on-send t)
; 	(setq w32-quote-process-args ?\")
; 	(make-variable-buffer-local 'comint-completion-addsuffix))

;       ;(setq shell-mode-hook 'my-shell-setup)
;       (setq process-coding-system-alist (cons '("bash" . raw-text-unix)
; 					      process-coding-system-alist))))


;;; =============================================================
;;; Syntax Highlighting, Spell Checking, Completion
;;; (ftp://ftp.hanq.net/pub/gnu/ (Source Code))
;;; (http://www.cat.rpi.edu/~tibbetts/ispell_toc.html (Windows 95/NT))
;;; =============================================================

(global-font-lock-mode 1)

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

(defun turn-on-flyspell-mode-program ()
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

;;; =============================================================
;;; Text Mode
;;; =============================================================

(setq default-major-mode (quote text-mode))

(add-hook 'text-mode-hook
          (function (lambda ()
                      (setq fill-column 80)
                      (turn-on-auto-fill))))

;; bug fix for ugly indentation by auto-fill-mode in emacs-22.0.50.1
(if (= emacs-major-version 22)
    (setq fill-prefix nil))
  
;;; =============================================================
;;; CSS Mode (http://www.stud.ifi.uio.no/~lmariusg/download/css-mode-doc.html)
;;; =============================================================

(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
;(setq cssm-indent-function #'cssm-c-style-indenter) 

;;; =============================================================
;;; Programming Modes
;;; =============================================================

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


;;; =============================================================
;;; Source code analysis and browsing
;;; CScope: http://cscope.sourceforge.net/
;;; CFlow: http://www.gnu.org/software/cflow
;;; =============================================================

;; Cscope
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


;;; =============================================================
;;; Compilation Modes
;;; =============================================================

(if (eq system-type 'windows-nt)
    (progn
      (setq-default grep-command "grep -n -e")
      (setq-default grep-find-command 
                    '("find . -type f -exec grep -n -e {} NUL ;" . 33))
      (setq-default grep-find-use-xargs nil)
      ))


;;; =============================================================
;;; Version Control
;;; http://svn.collab.net/repos/svn/trunk/contrib/client-side/psvn/psvn.el
;;; =============================================================

(setq vc-make-backup-files t)

(condition-case nil
    (require 'psvn)
  (error nil))


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
;;; Calcualtor
;;; =============================================================
;;; GNU Emacs 22 includes Calc.
(if (<= emacs-major-version 21)
    (progn
      (autoload 'calc-dispatch	   "calc" "Calculator Options" t)
      (autoload 'full-calc		   "calc" "Full-screen Calculator" t)
      (autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
      (autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
      (autoload 'defmath		   "calc" nil t t)
      (autoload 'calc			   "calc" "Calculator Mode" t)
      (autoload 'quick-calc		   "calc" "Quick Calculator" t)
      (autoload 'calc-keypad		   "calc" "X windows Calculator" t)
      (autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
      (autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
      (autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
      (autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
      (global-set-key "\e#" 'calc-dispatch)
      ))
;;; End of Calc autoloads.


;;; =============================================================
;;; Chinese Calendar (Joo ChurlSoo <vimacs@medical.co.kr>)
;;; =============================================================
(require 'cal-china)
(defun from-chinese-to-gregorian (date-list)
  "date-list is '(m d) or '(m d y) in chinese date"
  (let* ((date (if (= (length date-list) 2)
		   (append '(12 31)
			    (cdr (cdr (calendar-current-date))))
		 (append '(12 31) (cdr (cdr date-list)))))
	 (c-date (calendar-chinese-from-absolute
			    (calendar-absolute-from-gregorian date)))
	 (cycle (car c-date))
	 (year (car (cdr c-date)))
	 (month (car date-list))
	 (day (car (cdr date-list))))
    (calendar-gregorian-from-absolute (calendar-absolute-from-chinese
				       (list cycle year month day)))))

(setq korean-new-year-day (from-chinese-to-gregorian '(1 1)))
(setq buddha-day (from-chinese-to-gregorian '(4 8)))
(setq korean-thanksgiving-day (from-chinese-to-gregorian '(8 15)))
(setq other-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed (extract-calendar-month korean-new-year-day)
		       (extract-calendar-day korean-new-year-day)
		       "Lunar New Year's Day")
	(holiday-fixed 3 1 "Anniversary of the Samil Independence Movement")
	(holiday-fixed 4 5 "Arbor Day")
	(holiday-fixed 5 5 "Children's Day")
	(holiday-fixed (extract-calendar-month buddha-day)
		       (extract-calendar-day buddha-day)
		       "Buddha's Day")
	(holiday-fixed 6 6 "Memorial Day")
	(holiday-fixed 7 7 "Constitution Day")
	(holiday-fixed 8 15 "Independence Day of Korea")
	(holiday-fixed (extract-calendar-month korean-thanksgiving-day)
		       (extract-calendar-day korean-thanksgiving-day)
		       "Korean Thanksgiving Day(Harvest Moon Day)")
	(holiday-fixed 10 3 "Anniversary of Dangun's Accession")
	(holiday-fixed 12 25 "Christmas Day")))
(setq local-holidays '((holiday-fixed 4 13 "Assemblyman Election Day")))
(setq calendar-holidays (append other-holidays local-holidays))



;;; =============================================================
;;; HTML, XML Mode (mainly for Emacs 22)
;;; =============================================================

(add-hook 'sgml-mode-hook (function cscope:hook))

;; bug fix for ugly indentation by auto-fill-mode in emacs 22
(add-hook 'sgml-mode-hook (function (lambda ()
    (set (make-local-variable 'adaptive-fill-mode) nil))))

(add-hook 'html-mode-hook (function (lambda ()
    (if (zerop (buffer-size))
	(progn
	  (set (make-local-variable 'sgml-xml-mode) t)
	  (tempo-template-html-skeleton)))

    ;; for automatic indentation after tag insertion
    (define-skeleton html-unordered-list
      "HTML unordered list tags."
      nil
      "<ul>" > \n
      "<li>" _ (if sgml-xml-mode "</li>") > \n
      "</ul>" >)
    (define-skeleton html-ordered-list
      "HTML ordered list tags."
      nil
      "<ol>" > \n
      "<li>" _ (if sgml-xml-mode "</li>") > \n
      "</ol>" >)
    (define-skeleton html-list-item
      "HTML list item tag."
      nil
      (if (bolp) nil '\n)
      "<li>" > _ (if sgml-xml-mode "</li>") )
    )))

(setq html-new-buffer-template
 '("<?xml version=\"1.0\" encoding=\"euc-kr\"?>\n"
   "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ko\" lang=\"ko\">\n"
   "<head>\n"
   "<meta http-equiv=\"Content-Type\" content='text/html; charset=\"euc-kr\"' />\n"
   "<title>" p "</title>\n"
   "</head>\n\n"
   "<body>\n"
   "<h1>" p "</h1>\n"
   p
   "\n<hr>\n"
   "<address>" html-helper-address-string "</address>\n"
   "</body>\n"
   "</html>\n"))

(require 'tempo)
(tempo-define-template "html-skeleton" html-new-buffer-template
		       nil
		       "Insert a skeleton for a HTML document")

;;; =============================================================
;;; HTML Helper Mode (http://www.santafe.edu/~nelson/tools/) (mainly for Emacs 21)
;;; =============================================================

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq html-helper-new-buffer-template html-new-buffer-template)
(add-hook 'html-helper-mode-hook (function cscope:hook))
(if (and (<= emacs-major-version 21) (boundp 'magic-mode-alist))
    (set-variable 'magic-mode-alist (append '(("\\(?:<\\?xml\\s +[^>]*>\\)?\\s *<\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\(?:!DOCTYPE\\s +[^>]*>\\s *<\\s *\\(?:!--\\(?:[^-]\\|-[^-]\\)*-->\\s *<\\)*\\)?[Hh][Tt][Mm][Ll]" . html-helper-mode))
                                            magic-mode-alist)))

(if (<= emacs-major-version 21)
    (setq auto-mode-alist (append '(("\\.s?html?$" . html-helper-mode))
;;                                ("\\.jsp$" . html-helper-mode)
                                    auto-mode-alist)))
(add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
(add-hook 'html-helper-mode-hook 'turn-on-font-lock)


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
(add-hook 'TeX-mode-hook 'turn-on-font-lock)

(if (eq system-type 'windows-nt)
    (setq TeX-view-style '(("^a5$" "yap %d -paper a5")
                           ("^landscape$" "yap %d -paper a4r -s 4")
                           ("." "yap %d"))))


;;; =============================================================
;;; CEDET - Collection of Emacs Development Environment Tools (http://cedet.sourceforge.net/)
;;; =============================================================

;(condition-case nil
;    (require 'cedet)
;  (error nil))

;;; =============================================================
;;; JDEE - Java Development Environment for Emacs (http://jdee.sunsite.dk/)
;;; =============================================================
;;; installation guide: copy `/usr/local/share/emacs/20.3/site-lisp/jde-2.1.3/jtags*' to `/usr/local/bin/'

;; caution!: 방문한 디렉토리마다 semantic.cache 파일을 남기는 문제가 있음.
;(condition-case nil
;    (require 'jde)
;  (error nil))

(if (eq system-type 'windows-nt)
    (setq jde-db-source-directories '("C:/jdk1.5.0/src/"))
  (setq jde-db-source-directories '("/usr/local/jdk1.5.0/src")))


;;; =============================================================
;;; Telnet (http://www.watchit.com/~iganza/jtelnet.html) for Windows 95/NT
;;; =============================================================
;; Install JDK from http://java.sun.com/
;; Change followings properly for your system configuration

(if (eq system-type 'windows-nt)
    (progn
      (setq jtelnet-java-path "java")
      (setq jtelnet-class-arg "-cp")
      (setq jtelnet-class-path
	    (concat
	     (apply 'concat (mapcar
			     (lambda (x) (if (= x ?/) "\\" (char-to-string x)))
			     data-directory))
	     "..\\site-lisp\\jtelnet"))
      (condition-case nil
	  (require 'jtelnet)
	(error nil))))
  

;;; =============================================================
;;; Private Settting and Customization
;;; =============================================================

;; To see available colors, run `M-x list-colors-dislay'

(if (not (eq system-type 'windows-nt))
    (server-start))

(setenv "CVSROOT" ":pserver:gdhong@rnd.mycompany.com:/home/cvs/tf")

; For automatically expanding tab key input to spaces.
(setq-default indent-tabs-mode nil) 

(setq-default ange-ftp-default-user "gdhong")

(setq html-helper-address-string 
  "<a href=\"http://gdhong.com\">Gildong Hong &lt;gdhong@yahoo.co.kr&gt;</a>")

(if (eq system-type 'windows-nt)
    (setq Info-default-directory-list
          (append Info-default-directory-list '("c:/cygwin/usr/info/" "c:/cygwin/usr/local/info/" "c:/cygwin/usr/share/info/"))))

(if (eq system-type 'windows-nt)
     (progn
       (setenv "PATH" (concat "c:\\bin;c:\\Program Files\\Subversion\\bin;c:\\cygwin\\bin;c:\\cygwin\\usr\\local\\bin;c:\\jdk1.5.0\\bin;c:\\WINDOWS\\Microsoft.NET\\Framework\\v2.0.50727\\;c:\\Program Files\\Microsoft.NET\\SDK\\v2.0\\Bin" (getenv "PATH")))
       (set-variable 'exec-path (append '("c:\\bin" "c:\\Program Files\\Subversion\\bin" "c:\\cygwin\\bin" "c:\\cygwin\\usr\\local\\bin") exec-path))
       ;(setq-default grep-find-command 
                     ;'("find . -type f -exec grep -n -e {} NUL ;" . 33))))
))

;;; =============================================================
;;; color setting
;;; =============================================================
(custom-set-faces
	'(font-lock-comment-face ((t (:foreground "DeepSkyBlue"))))
	'(font-lock-string-face ((t (:foreground "red"))))
	'(font-lock-keyword-face ((t (:foreground "yellow"))))
	'(font-lock-function-name-face ((t (:foreground "cyan"))))
	'(font-lock-variable-name-face ((t (:foreground "magenta"))))
	'(isearch ((t (:background "red"))))
	'(isearch-lazy-highlight-face ((t (:background "yellow"))))
)

