#!/bin/sh
# backup scripts - configs, org, master's thesis, sync with gdrive

# todo pandoc org notes so they're readable from phone / other pc
# todo check connection so it's not using my virgin mobile packets
# todo two-way copy of master's thesis stuff

# update arch packages list
pacman -Q | cut -d ' ' -f1 > ~/.arch-packages-list

# config files to ~/.dotfiles git repo
~/.dotfiles/.sync.sh

function rcs {
    rclone sync -L $@
}    
# sync everything with gDrive
rcs ~/.dotfiles gdrive:dotfiles
rcs ~/org gdrive:org
rcs ~/masters-thesis gdrive:masters-thesis
rcs ~/.emacs.d/snippets/org-mode gdrive:snippets/org-mode
rcs ~/.aspell.en.prepl
rcs ~/.aspell.en.pws
