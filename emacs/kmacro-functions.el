(fset 'km-c-param-align
      (lambda (&optional arg)
        "align parameters or arguments of the function in c as following:
void foo(int a,
         int b,
         int c);"
        (interactive "p")
        (kmacro-exec-ring-item (quote
                                ("m {%, , 
!( )	" 0 "%d")) arg)))
