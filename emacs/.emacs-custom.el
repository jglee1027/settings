(cond ((equal system-type 'darwin)
       (setq default-font-spec-eng (font-spec :family "Monaco" :size 19.0))
       (setq default-font-spec-kor (font-spec :family "AppleGothic" :size 19.0))
       (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2022.1/libexec/plantuml.jar"))
      (t
       (setq default-font-spec-eng (font-spec :family "envypn" :size 19.0))
       (setq default-font-spec-kor (font-spec :family "NanumSquareRound" :size 19.0))
       (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")))

(setq doc-view-resolution 300)
(set-face-foreground 'org-hide "#ffffff")
(when window-system
  (setq init-ui-hook '((lambda ()
                         (load-theme 'jglee t t)
                         (load-theme 'leuven t nil)))))
