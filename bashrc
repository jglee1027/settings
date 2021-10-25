#!/usr/bin/env bash

add_completion() {
    which $1 &> /dev/null || return;
    source <($1 completion $2)

    type -t complete &> /dev/null || return;

    if [ "$3" != "" ]; then
        complete -o default -F __start_${1} $3 &> /dev/null && alias $3="$1"
    fi
}

add_complete() {
    type -t complete &> /dev/null && $*
}

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
alias xt16='xterm -fa envypn -fs 16'

src=${BASH_SOURCE[0]}
if [ "$src" != "" ]; then
    settings_dir="$(dirname $src)"
    export PATH=$PATH:$settings_dir/bin
fi

LS_COLORS='rs=0:di=00;34:ln=00;36:mh=00:pi=40;33:so=00;35:do=00;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=00;32:*.tar=00;31:*.tgz=00;31:*.arc=00;31:*.arj=00;31:*.taz=00;31:*.lha=00;31:*.lz4=00;31:*.lzh=00;31:*.lzma=00;31:*.tlz=00;31:*.txz=00;31:*.tzo=00;31:*.t7z=00;31:*.zip=00;31:*.z=00;31:*.dz=00;31:*.gz=00;31:*.lrz=00;31:*.lz=00;31:*.lzo=00;31:*.xz=00;31:*.zst=00;31:*.tzst=00;31:*.bz2=00;31:*.bz=00;31:*.tbz=00;31:*.tbz2=00;31:*.tz=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.war=00;31:*.ear=00;31:*.sar=00;31:*.rar=00;31:*.alz=00;31:*.ace=00;31:*.zoo=00;31:*.cpio=00;31:*.7z=00;31:*.rz=00;31:*.cab=00;31:*.wim=00;31:*.swm=00;31:*.dwm=00;31:*.esd=00;31:*.jpg=00;35:*.jpeg=00;35:*.mjpg=00;35:*.mjpeg=00;35:*.gif=00;35:*.bmp=00;35:*.pbm=00;35:*.pgm=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.tiff=00;35:*.png=00;35:*.svg=00;35:*.svgz=00;35:*.mng=00;35:*.pcx=00;35:*.mov=00;35:*.mpg=00;35:*.mpeg=00;35:*.m2v=00;35:*.mkv=00;35:*.webm=00;35:*.ogm=00;35:*.mp4=00;35:*.m4v=00;35:*.mp4v=00;35:*.vob=00;35:*.qt=00;35:*.nuv=00;35:*.wmv=00;35:*.asf=00;35:*.rm=00;35:*.rmvb=00;35:*.flc=00;35:*.avi=00;35:*.fli=00;35:*.flv=00;35:*.gl=00;35:*.dl=00;35:*.xcf=00;35:*.xwd=00;35:*.yuv=00;35:*.cgm=00;35:*.emf=00;35:*.ogv=00;35:*.ogx=00;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS

add_completion kubectl bash k
add_completion helm bash h
add_completion oc bash

source $settings_dir/bash_prompt
kctx on
