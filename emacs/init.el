;;;; macros
;; define ignore-errors macro
(eval-when-compile
  (defmacro ignore-errors (&rest body)
    "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY."
    `(condition-case nil (progn ,@body) (error nil))))

(if (not (fboundp 'with-eval-after-load))
    (defmacro with-eval-after-load (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(eval-after-load ,feature
         '(progn ,@body))))

;; ======================================================================
;; MELPA
;; ======================================================================
(let ((elpa-dir "~/.emacs.d/elpa"))
  (unless (file-exists-p elpa-dir)
    (make-directory elpa-dir)))

(let ((entries (nconc (directory-files "~/settings/emacs/site-lisp" t)
                      (directory-files "~/.emacs.d/elpa" t "[^.]"))))
  (mapcar (lambda (entry)
            (if (equal (car (file-attributes entry)) t)
                (add-to-list 'load-path entry)))
          entries))

;;;; el-get
;; (if (file-exists-p "~/.emacs.d/el-get/el-get")
;;     (add-to-list 'load-path "~/.emacs.d/el-get/el-get"))

;; (require 'el-get nil t)

;;;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(ignore-errors
  (eval-when-compile
    (require 'use-package)))

(unless (featurep 'use-package)
  (defmacro use-package (name &rest args)
       (message (format "use-package: %s cannot be loaded" name))))

;; ======================================================================
;; General setting
;; ======================================================================

;; to run multiple emacs daemons on a single system
(setq server-use-tcp t)

;;;; tab
(setq c-basic-offset 4)
(setq default-tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))

(setq inhibit-splash-screen t)
(setq scalable-fonts-allowed t)
(setq compilation-scroll-output t)
(setq ns-pop-up-frames 'nil)

(setq-default transient-mark-mode t)
(show-paren-mode t)
(column-number-mode t)
(display-time-mode t)
(ffap-bindings)
;; (global-hl-line-mode t)
;; (ido-mode t)

(setq semantic-load-turn-everything-on t)
(setq vc-make-backup-files t)

(fset 'yes-or-no-p 'y-or-n-p)

(unless (or enable-multibyte-characters window-system)
  (standard-display-european t)
  (set-input-mode (car (current-input-mode))
                  (nth 1 (current-input-mode))
                  0))

;;;; shell environment
(setq-default shell-cd-regexp nil)
(setq-default shell-pushd-regexp nil)
(setq-default shell-popd-regexp nil)

;;;; secure on shell mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;; dired
(add-hook 'dired-load-hook
          (function (lambda()
                      (load "dired-x" nil)
                      (setq dired-guess-shell-alist-user
                            (list
                             '("\\.[aA][vV][iI]$" "mplayer")
                             '("\\.[eE][mM][lL]$" "thunderbird")
                             '("\\.[mM][kK][vV]$" "mplayer")
                             '("\\.[mM][pP]4$" "mplayer")
                             '("\\.[pP][dD][fF]$" "evince")
                             '("\\.[wW][mM][vV]$" "mplayer"))))))
(ignore-errors
  (load-library "dired"))

;;;; find-dired
(with-eval-after-load "find-dired"
  (setq find-ls-option '("-exec ls -ldh {} +". "")))

;;;; uniquify
(with-eval-after-load "uniquify"
  (setq uniquify-buffer-name-style 'forward))

;;;; dropdown-list
(ignore-errors
  (require 'dropdown-list))

(ignore-errors
  (require 'uniquify))

;;;; Key definition
(global-set-key [C-kanji] 'set-mark-command)
(global-set-key [?\S- ] 'toggle-input-method)
(global-set-key (kbd "C-x RET s") 'decode-coding-region)
(global-set-key (kbd "C-c C-s a") 'semantic-complete-analyze-inline)
(global-set-key (kbd "C-c /") 'semantic-ia-complete-symbol-menu)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C") 'recompile)
(global-set-key (kbd "C-c TAB") 'indent-relative)
(defun remove-all-blank-lines ()
  (interactive)
  (flush-lines "^$" (beginning-of-buffer))
  (query-replace-regexp "<\\(.*://.*\\)>" "\\1"))
(global-set-key (kbd "C-c $") 'remove-all-blank-lines)
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key (kbd "C-x <f5>")  'revert-buffer)
(global-set-key (kbd "C-x C-b")  'ibuffer)

;;;; tags
(fset 'find-next-tag "\C-u\256")
(fset 'find-prev-tag "\C-u-\256")
(global-set-key (kbd "M-8") 'find-next-tag)
(global-set-key (kbd "M-7") 'find-prev-tag)

;;;; gud

;; (defadvice gdb-setup-windows (after gdb-setup-more-windows activate)
;;   "Customization window layout."
;;   (delete-window
;;    (car (get-buffer-window-list (gdb-locals-buffer-name))))
;;   (if (and (functionp 'gdb-inferior-io-name)
;;         (get-buffer-window-list (gdb-inferior-io-name)))
;;    (delete-window
;;     (car (get-buffer-window-list (gdb-inferior-io-name)))))
;;   (delete-window
;;    (car (get-buffer-window-list (gdb-breakpoints-buffer-name)))))

(defun gdb-setup-custom-windows ()
  "Customization window layout."
  (interactive)
  (delete-window
   (car (get-buffer-window-list (gdb-locals-buffer-name))))
  (if (and (functionp 'gdb-inferior-io-name)
           (get-buffer-window-list (gdb-inferior-io-name)))
      (delete-window
       (car (get-buffer-window-list (gdb-inferior-io-name)))))
  (delete-window
   (car (get-buffer-window-list (gdb-breakpoints-buffer-name)))))

(defun gud-mode-common-keys()
  (message ">>> run gud-mode-common-keys")
  (global-set-key [f5] 'gud-step)
  (global-set-key (kbd "C-<f5>") 'gud-stepi)
  (global-set-key [f6] 'gud-next)
  (global-set-key (kbd "C-<f6>") 'gud-nexti)
  (global-set-key [f7] 'gud-finish)
  (global-set-key [f8] 'gud-cont)
  (global-set-key [f9] 'gud-break)
  (global-set-key [f12] 'gdb-many-windows)
  (global-set-key (kbd "C-x <f12>") 'gdb-setup-custom-windows)
  (global-set-key (kbd "M-+") 'gud-up)
  (global-set-key (kbd "M-_") 'gud-down))

(add-hook 'gdb-frames-mode-hook (lambda()
                                  (global-set-key (kbd "M-+") '(lambda()
                                                                 (interactive)
                                                                 (if (functionp 'gdb-frames-force-update)
                                                                     (gdb-frames-force-update))
                                                                 (gud-up 1)))
                                  (global-set-key (kbd "M-_") '(lambda()
                                                                 (interactive)
                                                                 (if (functionp 'gdb-frames-force-update)
                                                                     (gdb-frames-force-update))
                                                                 (gud-down 1)))))

(add-hook 'gdb-mode-hook (lambda()
                           (gud-mode-common-keys)
                           (setq gdb-display-io-nopopup t)))
(add-hook 'sdb-mode-hook 'gud-mode-common-keys)
(add-hook 'dbx-mode-hook 'gud-mode-common-keys)
(add-hook 'xdb-mode-hook 'gud-mode-common-keys)
(add-hook 'perldb-mode-hook 'gud-mode-common-keys)
(add-hook 'pdb-mode-hook 'gud-mode-common-keys)
(add-hook 'lldb-mode-hook 'gud-mode-common-keys)
(add-hook 'jdb-mode-hook (lambda()
                           (gud-mode-common-keys)
                           (setq comint-prompt-regexp "^> \\|^[^ ]+\\[[0-9]+\\] \\|^<[0-9]*> [a-zA-Z0-9-_$]*\\[[0-9]+\\] ")))


;;;; jde and jdibug
(with-eval-after-load "jde"
  (ignore-errors
    (global-set-key [f5] 'jde-debug-step-into)
    (global-set-key [f6] 'jde-debug-step-over)
    (global-set-key [f7] 'jde-debug-step-out)
    (global-set-key [f8] 'jde-debug-cont)
    (global-set-key (kbd "M-+") 'jde-debug-up)
    (global-set-key (kbd "M-_") 'jde-debug-down)
    (require 'jdibug)
    (global-set-key [f5] 'jdibug-step-into)
    (global-set-key [f6] 'jdibug-step-over)
    (global-set-key [f7] 'jdibug-step-out)
    (global-set-key [f8] 'jdibug-resume)))

;;;; gdb-bp-session
(with-eval-after-load "gud"
  (ignore-errors
    (require 'gdb-bp-session)))

;;;; hs-minor-mode
(add-hook 'hs-minor-mode-hook
          (lambda()
            (local-set-key (kbd "C-c ' '") 'hs-toggle-hiding)
            (local-set-key (kbd "C-c ' ;") 'hs-hide-level)
            (local-set-key (kbd "C-c ' h") 'hs-hide-block)
            (local-set-key (kbd "C-c ' j") 'hs-show-block)
            (local-set-key (kbd "C-c ' k") 'hs-hide-all)
            (local-set-key (kbd "C-c ' l") 'hs-show-all)))

;;;; image-mode
(add-hook 'image-mode-hook
          (lambda()
            (define-key image-mode-map "w" 'image-transform-fit-to-width)
            (define-key image-mode-map "h" 'image-transform-fit-to-height)))

;;;; smerge-mode
(add-hook 'smerge-mode-hook
          (lambda()
            (define-key smerge-mode-map (kbd "C-c RET") 'smerge-keep-current)
            (define-key smerge-mode-map (kbd "C-c C") 'smerge-combine-with-next)
            (define-key smerge-mode-map (kbd "C-c E") 'smerge-ediff)
            (define-key smerge-mode-map (kbd "C-c R") 'smerge-refine)
            (define-key smerge-mode-map (kbd "C-c a") 'smerge-keep-all)
            (define-key smerge-mode-map (kbd "C-c b") 'smerge-keep-base)
            (define-key smerge-mode-map (kbd "C-c m") 'smerge-keep-mine)
            (define-key smerge-mode-map (kbd "C-c n") 'smerge-next)
            (define-key smerge-mode-map (kbd "C-c o") 'smerge-keep-other)
            (define-key smerge-mode-map (kbd "C-c p") 'smerge-prev)
            (define-key smerge-mode-map (kbd "C-c r") 'smerge-resolve)
            (define-key smerge-mode-map (kbd "C-c <") 'smerge-diff-base-mine)
            (define-key smerge-mode-map (kbd "C-c =") 'smerge-diff-mine-other)
            (define-key smerge-mode-map (kbd "C-c >") 'smerge-diff-base-other)))

;;;; highlight-symbol
(global-set-key (kbd "C-c j 8") 'highlight-symbol-mode)
(global-set-key (kbd "C-c *") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-c 8") 'highlight-symbol-at-point)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
(global-set-key (kbd "M-n") 'highlight-symbol-next)

(autoload 'highlight-symbol-mode "highlight-symbol" nil t)
(autoload 'highlight-symbol-at-point "highlight-symbol" nil t)
(autoload 'highlight-symbol-prev "highlight-symbol" nil t)
(autoload 'highlight-symbol-next "highlight-symbol" nil t)

;;;; whitespace-mode
(setq whitespace-display-mappings
      '(;; not display in fixed-width
        ;; (space-mark 32 [?\␣])
        ;; (newline-mark 10 [?¶ ?\n])
        (tab-mark 9 [?» ?\t])))

(add-hook 'whitespace-mode-hook
          (function (lambda()
                      (set-face-attribute 'whitespace-tab
                                          nil
                                          :background "magenta"
                                          :foreground nil)
                      (set-face-attribute 'whitespace-space
                                          nil
                                          :background nil
                                          :foreground "gray"))))

;;;; winmove
(global-set-key (kbd "C-x <left>") '(lambda ()
                                      (interactive)
                                      (ignore-errors
                                        (windmove-left))))
(global-set-key (kbd "C-x <right>") '(lambda ()
                                       (interactive)
                                       (ignore-errors
                                         (windmove-right))))
(global-set-key (kbd "C-x <up>") '(lambda ()
                                    (interactive)
                                    (ignore-errors
                                      (windmove-up))))
(global-set-key (kbd "C-x <down>") '(lambda ()
                                      (interactive)
                                      (ignore-errors
                                        (windmove-down))))

;;;; Mode line and minibuffer
(setq display-time-string-forms
      '((if (and
             (not display-time-format)
             display-time-day-and-date)
            (decode-coding-string (format-time-string "%m/%d (%a) " now)
                                  (car default-process-coding-system))
          "")
        (decode-coding-string
         (format-time-string
          (or display-time-format
              (if display-time-24hr-format "%H:%M" "%p %-I:%M"))
          now)
         (car default-process-coding-system))
        load
        (if mail " Mail" "")))

(setq display-time-day-and-date t)
(display-time)

;; ======================================================================
;; use-package
;; ======================================================================
(use-package auto-complete
  :disabled
  :ensure t
  :config
  (add-to-list 'ac-modes 'jde-mode)
  (add-to-list 'ac-modes 'objc-mode)
  (global-auto-complete-mode t))

(use-package auto-complete-yasnippet
  :disabled
  :ensure t
  :requires (yasnippet auto-complete)
  :config
  (setq-default ac-sources
                '(ac-source-yasnippet
                  ac-source-abbrev
                  ac-source-dictionary
                  ac-source-words-in-same-mode-buffers)))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package cmake-ide
  :disabled
  :ensure t
  :config
  (cmake-ide-setup))

(use-package cmake-mode
  :ensure t
  :config
  (setq cmake-tab-width 4))

(use-package docker-compose-mode
  :ensure t)

(use-package docker-tramp
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(defun helm-grep-do-git-grep-prompt (arg)
  (interactive "P")
  (require 'helm-files)
  (helm-grep-git-1 (helm-advice--ffap-read-file-or-url "Helm git grep dir: "
                                                       default-directory)
                   arg))
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h g") 'helm-grep-do-git-grep-prompt)
  (global-set-key (kbd "C-c h G") 'helm-grep-do-git-grep)
  (global-set-key (kbd "C-c o") 'helm-occur)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x M-f") 'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package helm-ag
  :ensure t
  :config
  (global-set-key (kbd "C-c h a") 'helm-do-grep-ag)
  (global-set-key (kbd "C-c h A") 'helm-do-ag-project-root))

(use-package helm-company
  :ensure t
  :requires (helm company)
  :config
  (define-key company-mode-map (kbd "C-c .") 'helm-company)
  (define-key company-active-map (kbd "C-c .") 'helm-company))

(use-package helm-gtags
  :ensure t
  :requires helm
  :custom
  (helm-gtags-auto-update t)
  (helm-gtags-ignore-case t)
  (helm-gtags-prefix-key "C-h g")
  (helm-gtags-suggested-key-mapping t)
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode))

(use-package indent-guide
  :ensure t
  :custom
  (indent-guide-char " ")
  :custom-face
  (indent-guide-face ((t (:inherit highlight))))
  :config
  (indent-guide-global-mode))

(use-package irony
  :disabled
  :ensure nil
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :disabled
  :ensure t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package js2-mode
  :ensure t)

(use-package kubernetes
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (perl-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :config
  (global-set-key (kbd "<f5>") 'dap-step-in)
  (global-set-key (kbd "<f6>") 'dap-next)
  (global-set-key (kbd "<f7>") 'dap-step-out)
  (global-set-key (kbd "<f8>") 'dap-continue)
  (global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
  (global-set-key (kbd "<f12>") 'dap-ui-show-many-windows)
  (global-set-key (kbd "C-<f12>") 'dap-ui-hide-many-windows)
  (global-set-key (kbd "M-+") 'dap-up-stack-frame)
  (global-set-key (kbd "M-_") 'dap-down-stack-frame)
  (global-set-key (kbd "C-c d D") 'dap-disconnect)
  (global-set-key (kbd "C-c d d") 'dap-debug)
  (global-set-key (kbd "C-c d l") 'dap-debug-last)
  (global-set-key (kbd "C-c d r") 'dap-debug-restart)
  (global-set-key (kbd "C-c d e") 'dap-ui-repl)
  (global-set-key (kbd "C-c d h") 'dap-hydra)
  (global-set-key (kbd "C-c d t") 'dap-tooltip-at-point))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x v g")
                  '(lambda()
                     (interactive)
                     (if (functionp 'magit-blame-popup)
                         (magit-blame-popup)
                       (magit-blame-mode)))))

(use-package markdown-mode
  :ensure t)

(use-package mu4e-views
  :ensure t
  :after mu4e
  :defer nil
  :bind
  (:map mu4e-headers-mode-map
        ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
        ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
        ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
        ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
        ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
        )
  :config
  ;; (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  ;; (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  ;; (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view

(use-package org
  :pin nongnu
  :ensure org-contrib
  :config
  (require 'ox-confluence)
  (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "")
                                         ("#+begin_src" . "")
                                         ("#+END_SRC" . "")
                                         ("#+end_src" . "")))
  (add-hook 'org-mode-hook 'prettify-symbols-mode)
  :custom
  (org-time-stamp-custom-formats '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
  (org-agenda-format-date "%Y-%m-%d %a")
  :bind
  (:map org-mode-map
        ("C-c C-x i" . org-time-stamp)))

(use-package org-bullets
  :ensure t
  :custom
  (org-ascii-bullets '((ascii ?* ?+ ?-) (latin1 ?* ?+ ?-) (utf-8 ?⦿ ?▶ ?■ ?▲ ?● ?▷ ?□ ?△ ?○)))
  (org-ascii-text-width most-positive-fixnum)
  (org-export-preserve-breaks t)
  (org-bullets-bullet-list '("⦿" "▶" "■" "▲" "●" "▷" "□" "△" "○"))
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package ox-reveal
  :ensure t
  :custom
  (org-reveal-extra-css (format "file://%s/settings/reveal/scrollable.css" (getenv "HOME"))))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (enable-paredit-mode)
              (local-set-key (kbd "C-c RET") 'eval-print-last-sexp)
              (local-set-key (kbd "C-c e") 'eval-print-last-sexp)))
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-default-exec-mode 'executable)
  :config
  (setq plantuml-output-type "png"))

(use-package s3ed
  :ensure t)

(use-package restclient
  :pin melpa
  :ensure t)

(use-package skewer-mode
  :pin melpa-stable
  :ensure t
  :config
  (skewer-setup))

;; slime
(use-package slime
  :ensure slime-company
  :bind
  (:map sldb-mode-map
        ("<f5>" . sldb-step)
        ("<f6>" . sldb-next)
        ("<f7>" . sldb-out)
        ("<f8>" . sldb-continue)
   :map slime-mode-map
        ("M-p" . nil)   ; slime-previous-note
        ("M-n" . nil))  ; slime-next-note

  :config
  (cond ((eq system-type 'darwin )
         (setq inferior-lisp-program "/usr/local/bin/sbcl"))
        (t
         (setq inferior-lisp-program "sbcl")))
  (setq slime-contribs '(slime-scratch slime-editing-commands slime-fancy slime-company)))

(use-package switch-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'switch-window))

(defun term-send-reset-ps1 ()
    "Send \'unset PROMPT_COMMAND; export PS1=xxx\' in term mode"
  (interactive)
  (term-send-raw-string "unset PROMPT_COMMAND
export PS1=\"\\e[7m\\u@\\h \\w\\n\\e[0m$ \"\n"))

(use-package term
  :ensure t
  :config
  (define-key term-raw-escape-map (kbd "C-l") 'term-send-reset-ps1))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup)
  :ensure t)

(use-package xcscope
  :ensure t
  :config
  (cscope-setup))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/settings/emacs/snippets")
  (add-to-list 'yas-snippet-dirs "~/settings/emacs/site-lisp/yasnippet/snippets")
  (setq yas/prompt-functions (cons 'yas/dropdown-prompt
                                   (remove 'yas/dropdown-prompt
                                           yas/prompt-functions)))
  (yas-global-mode 1))

;; ======================================================================
;; Programming modes
;; ======================================================================
;;;; auto-mode-alist
(setq magic-mode-alist nil)             ; to use eruby-nxhtml-mumamo-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.j$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cflow$" . cflow-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo-mode))
(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))

;;;; choose header file mode
(defun header-file-mode-hook()
  (if (and (file-name-extension buffer-file-name)
           (string-match "\\.[hH]$" buffer-file-name))
      (let ((filename (file-name-sans-extension buffer-file-name))
            (mode-alist '((".c" . c-mode)
                          (".C" . c-mode)
                          (".cpp" . c++-mode)
                          (".CPP" . c++-mode)
                          (".m" . objc-mode)
                          (".M" . objc-mode)
                          (".mm" . objc-mode)
                          (".MM" . objc-mode)))
            (mode nil))
        (dolist (ext-mode mode-alist)
          (if (file-exists-p (concat filename (car ext-mode)))
              (setq mode ext-mode)))

        (cond (mode
               (funcall (cdr mode))
               (hack-dir-local-variables-non-file-buffer))))))

(add-hook 'find-file-hook 'header-file-mode-hook)

(c-add-style
 "my-c-style"
 '("stroustrup"
   (c-offsets-alist
    (innamespace . 0))))

(c-add-style
 "java"
 '((c-basic-offset . 4)
   (c-comment-only-line-offset 0 . 0)
   (c-hanging-comment-starter-p)
   (c-offsets-alist
    (topmost-intro-cont . +)
    (statement-block-intro . +)
    (substatement-open . 0)
    (substatement . +)
    (inline-open . 0)
    (label . 0)
    (statement-case-open . +)
    (statement-cont . +)
    (knr-argdecl-intro . 5)
    (arglist-intro . +)
    (arglist-close . c-lineup-arglist)
    (brace-list-entry . 0)
    (access-label . 0)
    (inher-cont . c-lineup-java-inher)
    (func-decl-cont . c-lineup-java-throws))))

(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (setq abbrev-mode nil))))

(add-hook 'java-mode-hook
          (function (lambda ()
                      (c-set-style "java"))))

(add-hook 'jde-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-v .") 'jde-complete-minibuf)))

(add-hook 'c-mode-hook
          (function (lambda ()
                      (c-set-style "my-c-style"))))

(add-hook 'c++-mode-hook
          (function (lambda ()
                      (c-set-style "my-c-style"))))

;;;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(with-eval-after-load "highlight-sexp"
  (setq hl-sexp-background-color (if window-system
                                     "#00008b"
                                   "#ffffd7"))
  (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode))

(with-eval-after-load "c-eldoc"
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode))

(add-hook 'objc-mode-hook
          (lambda ()
            (c-set-style "my-c-style")
            (hi-lock-face-buffer "\\<\\(@property\\|@synthesize\\)\\>"
                                 font-lock-keyword-face)))

(add-hook 'idl-mode-hook
          (function (lambda ()
                      (c-set-style "stroustrup"))))

(add-hook 'python-mode-hook
          (lambda()
            (setq python-indent-offset 4)
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq-default python-indent 4)))

;;;; JDE
(defun jde-activate ()
  "Activates JDEE"
  (interactive)
  (require 'ecb)
  (require 'jde))

;;;; Android
(autoload 'android-jdb "android" nil t)
(autoload 'android-emulate "android" nil t)
(autoload 'android-install-app "android" nil t)
(autoload 'android-uninstall-app "android" nil t)
(autoload 'android-start-activity "android" nil t)
(autoload 'android-debug-activity "android" nil t)

;;;; nxhtml
(defun nxhtml-activate ()
  "Activates nXhtml"
  (interactive)
  (ignore-errors
    (require 'autostart)))

;;;; geben
(with-eval-after-load "geben"
  (define-key geben-mode-map [f5] 'geben-step-into)
  (define-key geben-mode-map [f6] 'geben-step-over)
  (define-key geben-mode-map [f7] 'geben-step-out)
  (define-key geben-mode-map [f8] 'geben-run))

(autoload 'geben "geben" "DBGp protocol front-end" t)

;;;; javascript
(autoload 'javascript-mode "javascript")

;;;; ruby
(defun which-gem-package(package)
  (let (start end path version)
    (catch 'which-gem-exception
      (shell-command (format "gem list -d %s" package) "*which-gem-package*")
      (with-current-buffer "*which-gem-package*"
        (goto-char (point-min))
        (if (null (re-search-forward "Installed at: " nil t))
            (throw 'which-gem-exception nil))
        (setq start (point))
        (if (null (re-search-forward "$" nil t))
            (throw 'which-gem-exception nil))
        (setq end (point))
        (setq path (buffer-substring start end))

        (goto-char (point-min))
        (if (null (re-search-forward (format "%s \(" package) nil t))
            (throw 'which-gem-exception nil))
        (setq start (point))
        (if (null (re-search-forward "\)$" nil t))
            (throw 'which-gem-exception nil))
        (setq end (1- (point)))
        (setq version (buffer-substring start end))

        (setq path (format "%s/gems/%s-%s" path package version))))
    (kill-buffer "*which-gem-package*")
    path))

(with-eval-after-load "ruby-mode"
  (ignore-errors
    (setq ri-ruby-script
          (locate-library "ri-emacs"))
    (load-library "ri-ruby")
    (add-hook 'ruby-mode-hook
              (lambda()
                (local-set-key (kbd "C-c h") 'ri)
                (local-set-key (kbd "C-c @") 'ri-ruby-show-args))))
  (ignore-errors
    (let ((rcodetools-path (which-gem-package "rcodetools")))
      (cond ((not (null rcodetools-path))
             (add-to-list 'load-path rcodetools-path)
             (require 'anything-rcodetools)
             (define-key ruby-mode-map (kbd "C-c /") 'rct-complete-symbol)))))
  (ignore-errors
    (require 'inf-ruby)
    (require 'ruby-electric)
    (require 'rdebug)))

(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'rdebug "rdebug" nil t)
(autoload 'rubydb "rubydb" nil t)
(autoload 'inf-ruby "inf-ruby" nil t)
(autoload 'ruby-electric-brace "ruby-electric" nil t)

;;;; rdebug keys
(defun rdebug-keys (map)
  (define-key map [f5] 'rdebug-step)
  (define-key map (kbd "C-<f5>") 'gud-stepi)
  (define-key map [f6] 'rdebug-next)
  (define-key map (kbd "C-<f6>") 'gud-nexti)
  (define-key map [f7] 'gud-finish)
  (define-key map [f8] 'gud-cont)
  (define-key map [f12] 'rdebug-restore-debugger-window-layout)
  (define-key map (kbd "M-+") 'gud-up)
  (define-key map (kbd "M-_") 'gud-down)
  (define-key map [f9]    'rdebug-toggle-source-breakpoint)
  (define-key map [C-f9]  'rdebug-toggle-source-breakpoint-enabled))
(setq rdebug-populate-common-keys-function 'rdebug-keys)

;;;; rinari
(autoload 'rinari-activate "rinari" nil t)

;;;; xcode-document-viewer
(with-eval-after-load "xcode-document-viewer"
  (setq xcdoc:document-path "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone4_0.iPhoneLibrary.docset"))

(autoload 'xcdoc:set-document-path "xcode-document-viewer" nil t)
(autoload 'xcdoc:search "xcode-document-viewer" nil t)
(autoload 'xcdoc:ask-search "xcode-document-viewer" nil t)
(autoload 'xcdoc:search-at-point "xcode-document-viewer" nil t)

;;;; w3m
(autoload 'w3m "w3m" nil t)
(autoload 'w3m-browse-url "w3m" nil t)

;;;; ecb
(autoload 'ecb-activate "ecb" nil t)

;;;; doxymacs
(ignore-errors
  (require 'doxymacs)
  (add-hook 'c-mode-common-hook 'doxymacs)
  (add-hook 'java-mode-hook 'doxymacs))

;;;; dsvn
(autoload 'svn-status "dsvn" "Run 'svn status'." t)
(autoload 'svn-update "dsvn" "Run 'svn update'." t)

;;;; svn
(defun jda-svn-log-report ()
  (interactive)
  (let (command
        id
        start-date
        end-date)
    (jda-gf-set-project-root)
    (setq id (read-from-minibuffer "Id: "))
    (setq start-date (read-from-minibuffer "Start date: "))
    (setq end-date (read-from-minibuffer "End date: "))
    (setq svnlr-rb (expand-file-name "svnlr.rb"
                                     (file-name-directory (symbol-file 'jda-svn-log-report))))
    (setq command (jda-read-shell-command
                   "Command: "
                   (format "cd %s; svn log | ~/settings/emacs/svnlr.rb -id %s -sd %s -ed %s"
                           jda-gf-project-root
                           id
                           start-date
                           end-date)))
    (shell-command command "*svn-log-report*")))

(global-set-key (kbd "C-x v #") 'jda-svn-log-report)

;;;; eclim
(ignore-errors
  (require 'eclim)
  (global-eclim-mode)

  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (require 'auto-complete)
  ;; regular auto-complete initialization
  (require 'auto-complete-config)
  (ac-config-default)

  ;; add the emacs-eclim source
  (require 'ac-emacs-eclim-source)
  (ac-emacs-eclim-config))

;;;; wgrep
(ignore-errors
  (require 'wgrep))

;;;; jda-minor-mode
(ignore-errors
  (require 'jda)
  (require 'jda-highlight-comment)
  (jda-minor-mode)
  (jda-hl-comment-mode))

;;;; docsetutil-el
(defcustom docsetutil-select-help-window t
  "Select help window after `docsetutil-search' funcstion is
finished."
  :type 'boolean
  :group 'docsetutil)

(defun docsetutil-search-wrap(&optional full-text)
  (interactive)
  (docsetutil-search (completing-read (format "Apple docset %s search: "
                                              (if full-text "full-text" "API"))
                                      (docsetutil-objc-completions)
                                      nil
                                      nil
                                      (current-word)
                                      'docsetutil-search-history (current-word))
                     full-text)
  (let ((help-buffer (get-buffer "*Help*")))
    (when (and docsetutil-select-help-window help-buffer)
      (select-window (get-buffer-window help-buffer)))))

(defun get-docsetutil-path ()
  (interactive)
  (or (executable-find "docsetutil")
      (with-temp-buffer
        (if (equal
             (shell-command-on-region (point-min)
                                      (point-max)
                                      "xcrun --find docsetutil"
                                      t)
             0)
            (buffer-substring (point-min) (- (point-max) 1))
          "docsetutil"))                ;not found
      ))

(defun set-docsetutil-path ()
  (interactive)
  (if (null (executable-find
             docsetutil-program))
      (setq-default docsetutil-program
                    (get-docsetutil-path))))

(defun docsetutil-search-api ()
  (interactive)
  (set-docsetutil-path)
  (docsetutil-search-wrap nil))

(defun docsetutil-search-full-text ()
  (interactive)
  (set-docsetutil-path)
  (docsetutil-search-wrap t))

(ignore-errors
  (require 'simple)
  (require 'docsetutil)
  (define-key help-map "D" 'docsetutil-search-full-text)
  (define-key help-map "d" 'docsetutil-search-api))

;; ======================================================================
;; Org-mode
;; ======================================================================
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(setq org-log-done t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "APPT(a@)" "|" "DONE(d!)" "CANCELED(c@)" "POSTPONED(p@)")
        (sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "REPORT(r)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f@)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "firebrick2" :weight bold))
        ("STARTED" . (:foreground "olivedrab"' :weight bold))
        ("WAITING" . (:foreground "sienna" :weight bold))
        ("APPT" . (:foreground "steelblue" :weight bold))
        ("DONE" . (:foreground "forestgreen" :weight bold))
        ("DEFERRED" . (:foreground "forestgreen" :weight bold))
        ("CANCELED" . shadow)))

(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("computer" . ?c)
                      ("project" . ?p)
                      ("reading" . ?r)
                      ("music" . ?m)
                      ("video" . ?v)))

(setq org-columns-default-format "%7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total} %38ITEM(Details) %TAGS(Context)")
(setq org-global-properties '(("Effort_All" . "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 8:00")))
(setq org-agenda-files (list "~/org/hobby.org"
                             "~/org/study.org"
                             "~/org/work.org"))
(setq org-clock-persist t)


(defun org-ctrl-c-underscore ()
  "Turn an item to a normal text"
  (interactive)
  (save-excursion
    (save-restriction
      (cond ((region-active-p)
             (narrow-to-region (region-beginning) (region-end))
             (call-interactively 'org-toggle-heading)
             (mark-whole-buffer)
             (call-interactively 'org-toggle-heading))
            (t
             (call-interactively 'org-toggle-heading)
             (call-interactively 'org-toggle-heading))))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c _") 'org-ctrl-c-underscore))

;;;; org-babel
(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (dot . t)))
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))
(add-to-list 'org-structure-template-alist
             '("u" "#+BEGIN_SRC plantuml :file ?.png
skinparam linetype ortho
#+END_SRC"))

;; ======================================================================
;; IRC
;; ======================================================================
(custom-set-variables
 ;; auto join
 '(erc-autojoin-channels-alist
   '(("hanirc.org"
      "#gnome"
      "#c++"
      "#java"
      "#eclipse"
      "#lisp"
      "#perl"
      "#python"
      "#ruby")
     ("freenode.net"
      "##c++"
      "##c++general"
      "##gnome"
      "##java"
      "##lisp"
      "##programming"
      "#android"
      "#android-dev"
      "#clim"
      "#clschool"
      "#debian"
      "#ecl"
      "#eclipse"
      "#emacs"
      "#haskell"
      "#linux"
      "#lisp"
      "#lispcafe"
      "#lispgames"
      "#lispweb"
      "#llvm"
      "#mezzano"
      "#perl"
      "#python"
      "#ruby"
      "#sbcl"
      "#scheme"
      "#shirakumo"
      "#sicl"
      "#slime"
      "#ubuntu")))
 ;; for hanirc.org
 '(erc-server-coding-system (quote (cp949 . undecided))))

(setq erc-server "irc.hanirc.org"
      erc-port 6667
      erc-nick "jjong"
      erc-user-full-name "The little prince"
      erc-password nil
      erc-prompt-for-password t)

(setq erc-server "irc.freenode.net"
      erc-port 8000
      erc-nick "jjong"
      erc-password nil
      erc-prompt-for-password t)

;; ======================================================================
;; Using ThingAtPoint and the Existing C-s C-w
;;
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; ======================================================================
(defun isearch-yank-symbol-from-beginning ()
  "Move to beginning of word before yanking word in isearch-mode."
  (interactive)
  ;; Making this work after a search string is entered by user
  ;; is too hard to do, so work only when search string is empty.
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'symbol))
  (isearch-yank-char (- (end-of-thing 'symbol) (beginning-of-thing 'symbol)))
  ;; Revert to 'isearch-yank-word-or-char for subsequent calls
  (substitute-key-definition 'isearch-yank-symbol-from-beginning
                             'isearch-yank-word-or-char
                             isearch-mode-map))

(add-hook 'isearch-mode-hook
          (lambda ()
            "Activate my customized Isearch word yank command."
            (substitute-key-definition 'isearch-yank-word-or-char
                                       'isearch-yank-symbol-from-beginning
                                       isearch-mode-map)))

;; ======================================================================
;; Misc. Customization
;; ======================================================================
(custom-set-variables
 '(compilation-message-face (quote default))
 '(compilation-skip-threshold 2) ;; Warnings and info
 '(show-paren-style (quote expression))
 '(hl-paren-background-colors
   (quote
    ("red" "green" "yellow" "blue" "magenta" "cyan" "white")))
 '(hl-paren-colors nil)
 '(align-c++-modes (quote (c++-mode c-mode java-mode objc-mode))))

;;;; compilation

;;; compilation-start
(defcustom compilation-start-display-output-buffer t
  "Enable or disable to display *compilation* bufffer on compilation-start.
If not-nil, *compilation* buffer is displayed."
  :type 'boolean
  :group 'compilation
  :version "20.4")
(defadvice compilation-start
    (around inhibit-display
            (command &optional mode name-function highlight-regexp))
  (if compilation-start-display-output-buffer
      ad-do-it
    (save-window-excursion
      ad-do-it)))
(ad-activate 'compilation-start)

;;; compilation-finish
(defun compilation-finish-switch-output-buffer (buffer outstr)
  (unless (string-match "finished" outstr)
    (switch-to-buffer-other-window buffer)))
(setq compilation-finish-functions 'compilation-finish-switch-output-buffer)

;;;; elpa
(defun install-elpa ()
  (interactive)
  (cond ((not (file-exists-p "~/.emacs.d/elpa/package.el"))
         (let ((buffer (url-retrieve-synchronously
                        "http://tromey.com/elpa/package-install.el")))
           (save-excursion
             (set-buffer buffer)
             (goto-char (point-min))
             (re-search-forward "^$" nil 'move)
             (eval-region (point) (point-max))
             (kill-buffer (current-buffer)))))
        (t
         (message "ELPA is already installed."))))

;;;; el-get-post-init-hooks
(add-hook 'el-get-post-init-hooks
          '(lambda (package)
             (cond ((eq package 'grep-a-lot)
                    (load-library "grep-a-lot")
                    (grep-a-lot-setup-keys)))))

(defun install-el-get ()
  (interactive)
  (cond ((null (require 'el-get nil t))
         (url-retrieve
          "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
          (lambda (s)
            (end-of-buffer)
            (eval-print-last-sexp))))
        (t
         (message "el-get alread installed."))))

;;;; platform specific settings
(defun default-font-get (font-spec-list)
  (catch 'while-exit
    (let (font)
      (while (not (null font-spec-list))
        (setq font (eval (pop font-spec-list)))
        (cond ((find-font font)
               (throw 'while-exit font)))))
    (throw 'while-exit nil)))

(defvar init-ui-hook '((lambda ()
                         (load-theme 'jglee t))))
(defun init-ui ()
  (interactive)
  (if (functionp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if window-system
      (menu-bar-mode 1)
    (menu-bar-mode -1))
  (run-hooks 'init-ui-hook))

(global-set-key (kbd "C-x C-l") 'init-ui)
;; (add-hook 'server-visit-hook 'init-ui)

(load-library "kmacro-functions.el")

(if (file-exists-p custom-file)
    (load custom-file))

(init-ui)
