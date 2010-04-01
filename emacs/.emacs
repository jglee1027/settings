(iswitchb-mode)
(which-function-mode)

(add-to-list 'load-path "~/settings/emacs/site-lisp")
(load-library "j-util")
(load-library "j-highlight")

 (if (eq system-type 'windows-nt)
 	(load-library "~/settings/emacs/windows/emacs")
 	(load-library "~/settings/emacs/linux/emacs"))
