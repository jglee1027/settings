(setq default-font-spec-eng (font-spec :family "envypn" :size 19.0))
(setq default-font-spec-kor (font-spec :family "NanumMyeongjo" :size 19.0))
(when window-system
  (setq init-ui-hook '((lambda ()
                         (load-theme 'jglee t t)
                         (load-theme 'whiteboard t nil)))))
