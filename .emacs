(setq custom-file "~/.emacs-custom.el")
(load-library "~/settings/emacs/init.el")
(if (file-exists-p custom-file)
	(load custom-file))
