(fset 'km-c-param-align
      (lambda (&optional arg)
        "align parameters or arguments of the function in c as following:
void foo(int a,
         int b,
         int c);"
        (interactive "p")
        (kmacro-exec-ring-item (quote
                                ([134217837 19 44 return 10 tab 134217837] 0 "%d"))
                               arg)))
