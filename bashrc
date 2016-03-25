#!/bin/bash

emacs_daemon() {
	if [ "$1" = "" ]; then
		echo "emacs error: server file required"
	else
		emacs --daemon=$1
	fi
}

emacs_client() {
	if [ "$1" = "" ]; then
		echo "emacsclient error: server file required"
	else
		emacsclient -nw --eval "(init-faces)" -f $1
	fi
}

emacs_client_frame() {
	if [ "$1" = "" ]; then
		echo "emacsclient error: server file required"
	else
		emacsclient -c --eval "(init-faces)" -f $1
	fi
}

emacs_gdb() {
	if [ "$1" = "" ]; then
		echo "emacs error: file required"
	else
		GDB_COMMAND="gdb -i=mi $1"
		emacs -nw --execute="(gdb \"$GDB_COMMAND\")"
	fi
}

export TERM=xterm-256color
export PS1="\[\033[01;32m\]\u@\h:\w\$(git branch 2>/dev/null | grep -e '\* ' | sed 's/^..\(.*\)/{\1}/')\[\033[00m\]\$ "

alias e='emacs_client'
alias ec='emacs_client_frame'
alias egdb='emacs_gdb'
alias en='emacs -nw'
alias es='emacs_daemon'
alias grep='grep --color=auto'
alias g='git'
alias gbl='git blame'
alias gbr='git branch'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log --format=fuller --decorate=full'
alias gpush='git push'
alias gpull='git pull'
alias gs='git status'
alias gsh='git show'
alias rm~='find . -iname "*~" | xargs rm -v'
alias yd='youtube-dl -f bestvideo+bestaudio'

settings_dir="$(dirname ${BASH_SOURCE[0]})"
export PATH=$PATH:$settings_dir/bin
