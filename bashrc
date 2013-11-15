export TERM=xterm-256color
export PS1="\[\033[01;32m\]\u:\w\$(git branch 2>/dev/null | grep -e '\* ' | sed 's/^..\(.*\)/{\1}/')\[\033[00m\]\$ "

alias e='emacsclient -nw'
alias en='emacs -nw'
alias grep='grep --color=auto'
