[ -f ~/.zshrc ] && . ~/.zshrc
export SBT_OPTS="-Xmx3G -XX:+UseG1GC -XX:+CMSClassUnloadingEnabled -Xss2M"

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  exec startx
fi
