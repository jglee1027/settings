(cond ((equal system-type 'darwin)
       (setq default-font-spec-eng (font-spec :family "Monaco" :size 19.0))
       (setq default-font-spec-kor (font-spec :family "AppleGothic" :size 19.0))
       (setq face-font-rescale-alist
             '(("AppleGothic" . 1.17)))
       (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.1/libexec/plantuml.jar"))
      ((equal system-type 'windows-nt)
       ;; (setq default-font-spec-eng (font-spec :family "나눔고딕_AndaleMono" :size 11.0))
       ;; (setq default-font-spec-kor (font-spec :family "나눔고딕_AndaleMono" :size 11.0))
       (setq default-font-spec-eng (font-spec :family "Consolas" :size 13.0))
       (setq default-font-spec-kor (font-spec :family "나눔고딕" :size 13.0))
       (setq face-font-rescale-alist
             '(("나눔고딕" . 1.17)))
       (setq org-plantuml-jar-path "c:/Program Files/plantuml/plantuml-1.2022.4.jar"))
      (t
       (setq default-font-spec-eng (font-spec :family "envypn" :size 19.0))
       (setq default-font-spec-kor (font-spec :family "NanumGothic" :size 22.8))
       ;; (setq face-font-rescale-alist
       ;;       '(("NanumGothic" . 1.2)))
       
       (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")))

(setq doc-view-resolution 300)
(set-face-foreground 'org-hide "#ffffff")
(when window-system
  (setq init-ui-hook '((lambda ()
                         (load-theme 'jglee t t)
                         (load-theme 'leuven t nil)))))
