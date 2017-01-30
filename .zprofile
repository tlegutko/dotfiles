[ -f ~/.zshrc ] && . ~/.zshrc
export PATH="$PATH:/home/tlegutko/.npm-global/bin"
if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
