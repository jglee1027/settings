(setq default-font-spec-eng (font-spec :family "envypn" :size 19.0))
(setq default-font-spec-kor (font-spec :family "NanumSquareRound" :size 19.0))
(setq doc-view-resolution 300)
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(set-face-foreground 'org-hide "#ffffff")
(when window-system
  (setq init-ui-hook '((lambda ()
                         (load-theme 'jglee t t)
                         (load-theme 'leuven t nil)))))
