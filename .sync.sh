#!/bin/sh

cd ~/.dotfiles
rsync -artuv --exclude ".git/" --exclude ".gitignore" . ~
rsync -artuv --files-from=.dotfiles-sync-list ~ .
