#!/usr/bin/env bash

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
        emacsclient -nw --eval "(init-ui)" -f $1
    fi
}

emacs_client_frame() {
    if [ "$1" = "" ]; then
        echo "emacsclient error: server file required"
    else
        emacsclient -c --eval "(init-ui)" -f $1
    fi
}

emacs_gdb() {
    if [ "$1" = "" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} FILE [ARG]..."
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} emacs"
    else
        GDB_COMMAND="gdb -i=mi $@"
        emacs -nw --execute="(gdb \"$GDB_COMMAND\")"
    fi
}

fcd() {
    local fcd_history="/tmp/fcd.history"
    local ignore_path_opts="\
-path *.git* -o \
-path *.svn* -o \
-path *.cvs* \
"

    if [ "$(which fzy)" == "" ]; then
        echo "Not found fzy!!!"
        echo "Install fzy as the following:"
        echo "$ sudo apt install fzy"
        return
    fi

    if [ "$1" == "-c" ]; then
        echo "fcd history was cleaned"
        rm -f $fcd_history*
        return
    fi

    local maxdepth=""
    if [ "$2" != "" ]; then
        maxdepth="-maxdepth $2"
    fi

    local dir="$(echo > /dev/null | tee \
>(cat $fcd_history 2> /dev/null) \
>(find $1 $maxdepth -type d ! \( $ignore_path_opts \) 2> /dev/null) \
> /dev/null | \
fzy -l 20)"

    if [ "$dir" == "" ]; then
        return
    fi

    cd  "$dir" &&
        (echo $PWD; cat $fcd_history 2> /dev/null) | awk '!x[$0]++' > $fcd_history.1
    mv $fcd_history.1 $fcd_history
}

is_utf_8_with_bom() {
    if [ "$1" = "" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} FILE"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} a.txt && echo yes"
        return
    fi

    file=$1
    head -c3 "$file" | grep -q $'\xef\xbb\xbf';
}

utf_8_with_bom() {
    if [ "$2" = "" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} FROM_ENCODING FILE"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} cp949 a.txt"
        return
    fi

    from_encoding=$1
    file=$2
    uconv -f $from_encoding -t utf-8 --add-signature $file > $file.utf8
    mv $file.utf8 $file
}

line_ending_lf_to_crlf() {
    if [ "$1" = "" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} FILE"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} a.txt"
        return
    fi

    file=$1
    perl -i -pe 's/([^\r])\n/$1\r\n/' "$file"
    perl -i -pe 's/^\n/\r\n/' "$file"
}

line_ending_crlf_to_lf() {
    if [ "$1" = "" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} FILE"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} a.txt"
        return
    fi

    file=$1
    perl -i -pe 's/\r\n/\n/' "$file"
}

cc_dump_macro() {
    echo "$1" | cc -dM -E -
}

cpp_dump_macro() {
    echo "$1" | cpp -dM -E -x c++ -
}

gr() {
    if [ "$1" = "--help" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} [REPOSITORY [REFSPEC...]]"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} origin"
        return 0
    fi

    local_branch=$(git rev-parse --abbrev-ref HEAD)
    if [ $? -ne 0 ]; then
        return 1
    fi

    repo=$1
    refspec=$2

    if [ "$repo" = "" ]; then
        repo=$(git config "branch.${local_branch}.remote")
        if [ "$repo" = "" ]; then
            echo "error: Not found upsteam of '${local_branch}' branch !!!"
            echo "Run \"git branch --set-upstream-to\" or \"gr REPOSITORY REFSPEC\""
            return 2
        fi

        remote_branch=$(git config "branch.${local_branch}.merge")
        remote_branch=${remote_branch##refs/heads/}
    fi

    if [ "$refspec" = "" ]; then
        refspec="$local_branch:refs/for/$remote_branch"
    fi

    echo -n "git push $repo $refspec$PUSH_OPTIONS (y or n) "
    read choice
    if [ "$choice" = "Y" -o "$choice" = "y" ]; then
        LANGUAGE=C git push $repo $refspec$PUSH_OPTIONS
    fi
}

yesorno() {
    echo -n "$* (y or n) "
    read choice
    if [ "$choice" = "Y" -o "$choice" = "y" ]; then
        $*
    fi
}

grho() {
    if [ "$1" = "--help" ]; then
        echo "SYNOPSIS"
        echo "   ${FUNCNAME[0]} [COMMIT]"
        echo "EXAMPLE"
        echo "   $ ${FUNCNAME[0]} origin/develop"
        return 0
    fi

    local_branch=$(git rev-parse --abbrev-ref HEAD)
    if [ $? -ne 0 ]; then
        return 1
    fi

    commit=$1
    if [ "$commit" = "" ]; then
        remote=$(git config "branch.${local_branch}.remote")
        if [ "$remote" = "" ]; then
            echo "error: Not found upsteam of '${local_branch}' branch !!!"
            echo "Run \"git branch --set-upstream-to\""
            return 2
        fi

        remote_branch=$(git config "branch.${local_branch}.merge")
        remote_branch=${remote_branch##refs/heads/}
        commit="$remote/$remote_branch"
    fi

    echo -n "git reset --hard $commit (y or n) "
    read choice
    if [ "$choice" = "Y" -o "$choice" = "y" ]; then
        LANGUAGE=C git reset --hard  $commit
    fi
}

lesstab() {
    sed $'s/\t/»   /g' | less -rX $@
}

gsh() {
    LANGUAGE=C git show --color --format=fuller $@ | lesstab
}

jsonindent() {
    python -m json.tool
}

export TERM=xterm-256color
export PS1="\[\033[01;32m\]\u@\h:\w\$(git branch 2>/dev/null | grep -e '\* ' | sed 's/^..\(.*\)/{\1}/')\[\033[00m\]\$ "

alias e='emacs_client'
alias ec='emacs_client_frame'
alias en='emacs -nw'
alias es='emacs_daemon'
alias grep='grep --color=auto'
alias g='LANGUAGE=C git'
alias gbl='LANGUAGE=C git blame -wM'
alias gbr='LANGUAGE=C git branch'
alias gd='LANGUAGE=C git diff --color | lesstab'
alias gdc='LANGUAGE=C git diff --color --cached | lesstab'
alias gg='LANGUAGE=C git grep'
alias gl='LANGUAGE=C git log --color --format=fuller --decorate=full'
alias gpush='LANGUAGE=C git push'
alias gpull='LANGUAGE=C git pull'
alias gr='LANGUAGE=C git-gr'
alias gs='LANGUAGE=C git status --short --branch'
alias l='ls -F'
alias ll='ls -alF'
alias m="make -j8"
alias rm#='find . -iname "#*#" -exec rm -v "{}" \;'
alias rm~='find . -iname "*~" -exec rm -v "{}" \;'
alias yd='youtube-dl -f bestvideo+bestaudio'
alias weather='curl -4 http://wttr.in/Pangyo,South_Korea'

src=${BASH_SOURCE[0]}
if [ "$src" != "" ]; then
    settings_dir="$(dirname $src)"
    export PATH=$PATH:$settings_dir/bin
fi
