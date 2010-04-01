(iswitchb-mode)
(which-function-mode)

(fset 'find-next-tag "\C-u\256")
(fset 'find-prev-tag "\C-u-\256")
(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)

(add-to-list 'load-path "~/settings/emacs/site-lisp")
(load-library "j-util")
(load-library "j-highlight")

(if (eq system-type 'windows-nt)
 	(load-library "~/settings/emacs/windows/emacs")
  (load-library "~/settings/emacs/linux/emacs"))
