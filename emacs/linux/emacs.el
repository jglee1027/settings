;; ======================================================================
;; Custom variables and faces
;; ======================================================================
(defvar default-font-spec-eng-list '((font-spec :family "Andale Mono" :size 10.0)
									 (font-spec :family "NanumGothic_AndaleMono" :size 10.0)
									 (font-spec :family "Liberation Mono" :size 10.0)
									 (font-spec :family "Monospace" :size 10.0)))

(defvar default-font-spec-kor-list '((font-spec :family "NanumGothic" :size 10.0)
									 (font-spec :family "UnBatang" :size 10.0)
									 (font-spec :family "Monospace" :size 10.0)))

(defvar default-font-spec-eng-mac-list '((font-spec :family "Andale Mono" :size 12.0)
										 (font-spec :family "Menlo" :size 12.0)
										 (font-spec :family "Monaco" :size 12.0)
										 (font-spec :family "Courier New" :size 12.0)))

(defvar default-font-spec-kor-mac-list '((font-spec :family "나눔고딕" :size 12.0)
										 (font-spec :family "AppleGothic" :size 12.0)
										 (font-spec :family "AppleMyungjo" :size 12.0)))

(when window-system
  (custom-set-variables
   '(scroll-bar-mode nil))
  
  (custom-set-faces
   '(default ((t (	:inherit nil
					:stipple nil
					:background "#102040"
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
					:foundry "outline"))))
   '(compilation-error ((t (:foreground "red"))))
   '(compilation-info ((((class color)) nil)))
   '(compilation-warning ((((class color)) (:foreground "yellow"))))
   '(cscope-line-face ((nil)))
   '(cscope-line-number-face ((nil)))
   '(cscope-mouse-face ((t (:inherit highlight))))
   '(cursor ((t (:background "yellow" :foreground "black"))))
   '(diff-added ((t (:foreground "green"))))
   '(diff-changed ((nil (:foreground "yellow"))))
   '(diff-context ((nil)))
   '(diff-file-header ((t (:foreground "magenta"))))
   '(diff-header ((t (:foreground "DeepSkyBlue3"))))
   '(diff-refine-change ((t (:background "yellow" :foreground "black"))))
   '(diff-removed ((t (:foreground "red"))))
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
   '(highlight ((t (:background "black" :foreground "white"))))
   '(isearch ((t (:background "green" :foreground "black"))))
   '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
   '(minibuffer-prompt ((t (:foreground "cyan"))))
   '(mode-line ((t (:background "gray" :foreground "black"))))
   '(mouse ((t nil)))
   '(mumamo-background-chunk-major ((t nil)))
   '(mumamo-background-chunk-submode1 ((t nil)))
   '(mumamo-background-chunk-submode2 ((t nil)))
   '(mumamo-background-chunk-submode3 ((t nil)))
   '(mumamo-background-chunk-submode4 ((t nil)))
   '(region ((t (:background "gray" :foreground "black")))))
  (cond ((equal system-type 'darwin)	; macosx
		 (set-face-font 'default (default-font-get default-font-spec-eng-mac-list))
		 (set-fontset-font nil 'korean-ksc5601 (default-font-get default-font-spec-kor-mac-list)))
		(t								; linux
		 (set-face-font 'default (default-font-get default-font-spec-eng-list))
		 (set-fontset-font nil 'korean-ksc5601 (default-font-get default-font-spec-kor-list))))
  )
(unless window-system
  (custom-set-faces
   '(compilation-error ((t (:foreground "red"))))
   '(compilation-info ((((class color)) nil)))
   '(compilation-warning ((((class color)) (:foreground "yellow"))))
   '(cscope-line-face ((nil)))
   '(cscope-line-number-face ((nil)))
   '(cscope-mouse-face ((t (:inherit highlight))))
   '(ecb-default-highlight-face ((t (:inherit highlight))))
   '(diff-added ((t (:foreground "green"))))
   '(diff-changed ((nil (:foreground "yellow"))))
   '(diff-context ((nil)))
   '(diff-file-header ((t (:foreground "magenta"))))
   '(diff-header ((t (:foreground "blue"))))
   '(diff-refine-change ((t (:background "yellow" :foreground "black"))))
   '(diff-removed ((t (:foreground "red"))))
   '(ecb-type-tag-class-face ((t (:foreground "green"))))
   '(ecb-type-tag-enum-face ((t (:foreground "green"))))
   '(ecb-type-tag-group-face ((t (:foreground "green"))))
   '(ecb-type-tag-interface-face ((t (:foreground "green"))))
   '(ecb-type-tag-struct-face ((t (:foreground "green"))))
   '(ecb-type-tag-typedef-face ((t (:foreground "green"))))
   '(ecb-type-tag-union-face ((t (:foreground "green"))))
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
   '(highlight ((t (:background "black" :foreground "white"))))
   '(isearch ((t (:background "green" :foreground "black"))))
   '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
   '(log-view-file ((t (:background "green" :foreground "black"))))
   '(log-view-message ((t (:background "white" :foreground "black"))))
   '(minibuffer-prompt ((t (:foreground "cyan"))))
   '(mumamo-background-chunk-major ((t nil)))
   '(mumamo-background-chunk-submode1 ((t nil)))
   '(mumamo-background-chunk-submode2 ((t nil)))
   '(mumamo-background-chunk-submode3 ((t nil)))
   '(mumamo-background-chunk-submode4 ((t nil)))
   '(region ((t (:background "white" :foreground "black")))))
  )
