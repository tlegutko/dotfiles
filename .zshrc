export TERMINAL="termite"
export VISUAL="vim"
export ZSH=/home/tlegutko/.oh-my-zsh
ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
COMPLETION_WAITING_DOTS="true"
# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"
plugins=(git)
source $ZSH/oh-my-zsh.sh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

bindkey -v
bindkey '^R' history-incremental-search-backward
setopt rmstarsilent

eval $(keychain --eval --quiet --nogui id_rsa)
