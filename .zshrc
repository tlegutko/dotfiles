export TERMINAL="termite"
export VISUAL="vim"
export ZSH=/home/tlegutko/.oh-my-zsh
ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"
source $ZSH/oh-my-zsh.sh

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

KEYTIMEOUT=1
alias em='emacsclient -t'
alias sd='~/.dotfiles/.sync.sh'

eval $(keychain --eval --quiet --nogui id_rsa)
