;; ======================================================================
;; Custom variables and faces
;; ======================================================================
(when window-system
  (set-background-color "#102050")
  (set-foreground-color "white")
  (set-face-font 'default
				 (font-spec :family "Andale Mono"
							:size 11.0))
  (set-fontset-font nil
					'korean-ksc5601
					(font-spec :family "NanumGothic_AndaleMono"
							   :registry "unicode-bmp"
							   :lang "ko"
							   :size 11.0))
  (custom-set-variables
   '(scroll-bar-mode (quote right)))
  (custom-set-faces
   '(compilation-info ((t (:foreground "DodgerBlue"))))
   '(compilation-warning ((t (:foreground "Orange"))))
   '(cscope-line-face ((nil)))
   '(cscope-line-number-face ((nil)))
   '(cscope-mouse-face ((t (:inherit highlight))))
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
