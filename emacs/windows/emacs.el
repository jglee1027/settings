;; ======================================================================
;; Custom variables and faces
;; ======================================================================
(defvar default-font-spec-eng (font-spec :family "Ubuntu Mono" :size 9.0))
(defvar default-font-spec-kor (font-spec :family "NanumMyeongjo" :size 9.0))

(defvar default-font-spec-eng-list '((font-spec :family "Ubuntu Mono" :size 9.0)
                                     (font-spec :family "Andale Mono" :size 9.0)
                                     (font-spec :family "Dina TTF" :size 9.0)
                                     (font-spec :family "���������ڵ�" :size 9.0)
                                     (font-spec :family "Bitstream Vera Sans Mono" :size 9.0)
                                     (font-spec :family "Consolas" :size 9.0)))

(defvar default-font-spec-kor-list '((font-spec :family "NanumMyeongjo" :size 9.0)
                                     (font-spec :family "NanumGothic" :size 9.0)
                                     (font-spec :family "���������ڵ�" :size 9.0)
                                     (font-spec :family "���� ����" :size 9.0)
                                     (font-spec :family "����" :size 9.0)))

(custom-set-variables
 '(scroll-bar-mode nil))

(custom-set-faces
 '(default ((t (:inherit nil
                :stipple nil
                :background "#103070"
                :foreground "#90c090"
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal
                :height 90
                :width normal
                :foundry "outline"))))
 '(compilation-error ((t (:foreground "red"))))
 '(compilation-info ((((class color)) nil)))
 '(compilation-line-number ((((class color)) nil)))
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
 '(ediff-current-diff-A ((t (:background "yellow" :foreground "black"))))
 '(ediff-current-diff-Ancestor ((t (:background "yellow" :foreground "black"))))
 '(ediff-current-diff-B ((t (:background "yellow" :foreground "black"))))
 '(ediff-current-diff-C ((t (:background "yellow" :foreground "black"))))
 '(ediff-even-diff-A ((t (:background "brightblack"))))
 '(ediff-even-diff-Ancestor ((t (:background "brightblack"))))
 '(ediff-even-diff-B ((t (:background "brightblack"))))
 '(ediff-even-diff-C ((t (:background "brightblack"))))
 '(ediff-fine-diff-A ((t (:background "blue" :foreground "black"))))
 '(ediff-fine-diff-Ancestor ((t (:background "blue" :foreground "black"))))
 '(ediff-fine-diff-B ((t (:background "blue" :foreground "black"))))
 '(ediff-fine-diff-C ((t (:background "blue" :foreground "black"))))
 '(ediff-odd-diff-A ((t (:background "brightblack"))))
 '(ediff-odd-diff-Ancestor ((t (:background "brightblack"))))
 '(ediff-odd-diff-B ((t (:background "brightblack"))))
 '(ediff-odd-diff-C ((t (:background "brightblack"))))
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
 '(font-lock-variable-name-face ((t (:foreground "tomato"))))
 '(fringe ((nil)))
 '(hi-blue ((t (:background "blue"))))
 '(highlight ((t (:background "black"))))
 '(hl-line ((t (:background "DodgerBlue4"))))
 '(isearch ((t (:background "green" :foreground "black"))))
 '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:inherit diff-added))))
 '(magit-diff-context-highlight ((t nil)))
 '(magit-diff-file-heading ((t (:inherit diff-header))))
 '(magit-diff-hunk-heading ((t (:inherit diff-hunk-header))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight ((t (:inherit diff-removed))))
 '(magit-item-highlight ((t (:inherit nil))))
 '(magit-selection-highlight ((t nil)))
 '(match ((t (:background "blue" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line ((t (:background "blue" :foreground "white"))))
 '(mode-line-inactive ((t (:background "black" :foreground "green"))))
 '(mouse ((t nil)))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((t nil)))
 '(mumamo-background-chunk-submode2 ((t nil)))
 '(mumamo-background-chunk-submode3 ((t nil)))
 '(mumamo-background-chunk-submode4 ((t nil)))
 '(region ((t (:background "gray" :foreground "black"))))
 '(show-paren-match ((t (:background "blue"))))
 '(whitespace-line ((t nil)))
 '(whitespace-space ((t (:background nil :foreground "yellow"))))
 '(whitespace-tab ((t (:background nil :foreground "yellow")))))

(with-eval-after-load ".emacs-custom.el"
  (cond ((find-font default-font-spec-eng)
         (set-face-font 'default default-font-spec-eng))
        (t
         (set-face-font 'default (default-font-get default-font-spec-eng-list))))
  (cond ((find-font default-font-spec-kor)
         (set-fontset-font nil 'korean-ksc5601 default-font-spec-kor))
        (t
         (set-fontset-font nil 'korean-ksc5601 (default-font-get default-font-spec-kor-list)))))
