#!/bin/bash

export TERM=xterm-256color
export PS1="\[\033[01;32m\]\u@\h:\w\$(git branch 2>/dev/null | grep -e '\* ' | sed 's/^..\(.*\)/{\1}/')\[\033[00m\]\$ "

alias e='emacsclient -nw --eval "(init-faces)"'
alias en='emacs -nw'
alias grep='grep --color=auto'
alias g='git'

dir="$(dirname ${BASH_SOURCE[0]})"
export PATH=$PATH:$dir
