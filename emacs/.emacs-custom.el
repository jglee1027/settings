;; (global-hl-line-mode t)

(ignore-errors
  (el-get-init "grep-a-lot"))

(ignore-errors
  (el-get-init "highlight-sexp")
  (load-library "highlight-sexp"))

(ignore-errors
  (el-get-init "c-eldoc")
  (load-library "c-eldoc"))
