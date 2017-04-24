#!/bin/sh
# backup scripts - configs, org, master's thesis, sync with gdrive
# todo pandoc org notes so they're readable from phone / other pc
# todo configs to .dotfiles and then no need for global git repo, stuff can be safely in .dotfiles
# todo check connection so it's not using my virgin mobile packets
# todo two-way copy of master's thesis stuff

rclone sync ~/org gdrive:org
rclone sync ~/masters-thesis gdrive:masters-thesis
rclone sync ~/.emacs.d/snippets/org-mode gdrive:snippets/org-mode
