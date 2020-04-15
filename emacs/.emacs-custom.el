;; (global-hl-line-mode t)

(ignore-errors
  (el-get-init "grep-a-lot"))

(ignore-errors
  (el-get-init "switch-window"))

(ignore-errors
  (el-get-init "magit")
  (load-library "magit")
  (el-get-init "magit-extras")
  (load-library "magit-extras"))

(ignore-errors
  (el-get-init "lua-mode")
  (load-library "lua-mode"))

(ignore-errors
  (el-get-init "cmake-mode")
  (load-library "cmake-mode")
  (setq cmake-tab-width 4))

(ignore-errors
  (el-get-init "markdown-mode")
  (load-library "markdown-mode"))

(ignore-errors
  (el-get-init "highlight-sexp")
  (load-library "highlight-sexp"))

(ignore-errors
  (el-get-init "c-eldoc")
  (load-library "c-eldoc"))

(ignore-errors
  (el-get-init "paredit")
  (load-library "paredit"))

(ignore-errors
  (el-get-init "helm")
  (load-library "helm"))

(ignore-errors
  (el-get-init "helm-company")
  (load-library "helm-company")
  (eval-after-load "company"
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))

(ignore-errors
  (el-get-init "helm-gtags")
  (custom-set-variables
   '(helm-gtags-auto-update t)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-prefix-key "C-h g")
   '(helm-gtags-suggested-key-mapping t))
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map
      (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map
      (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map
      (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map
      (kbd "M-g M-p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map
      (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map
      (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map
      (kbd "M-,") 'helm-gtags-pop-stack))
  (load-library "helm-gtags"))

(ignore-errors
  (el-get-init "pos-tip")
  (load-library "pos-tip")
  (el-get-init "company-quickhelp")
  (load-library "company-quickhelp")
  (company-quickhelp-mode))

;; slime
(ignore-errors
  (el-get-init 'slime)
  (setq inferior-lisp-program "/usr/bin/clisp")
  (setq slime-contribs '(slime-fancy)))

;; cmake-ide
(ignore-errors
  (el-get-init "s"))
(ignore-errors
  (el-get-init "rtags"))
(ignore-errors
  (el-get-init "cmake-ide")
  (load-library "rtags")
  (load-library "cmake-ide")
  (cmake-ide-setup))
