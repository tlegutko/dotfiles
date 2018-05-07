[ -f ~/.zshrc ] && . ~/.zshrc
export PATH="$PATH:/home/tlegutko/.npm-global/bin"
if [ -z "$DISPLAY" -a "$(tty)" = '/dev/tty2' ]; then
    exec xinit /home/tlegutko/.xinitrc-exwm -- vt02
elif [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
