export ZSH_THEME="simple"
export LESS="-Xr -x1,5"

source $ZSH/oh-my-zsh.sh

export PROMPT='%(!.%{$fg[red]%}.%{$fg[green]%})%~$(git_prompt_info)%{$reset_color%}'$'\n''$ '
