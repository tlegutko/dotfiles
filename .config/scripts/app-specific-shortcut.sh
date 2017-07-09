#!/bin/sh
# usage:
# ./app-specific-shortcut.sh lowercase-name pressed-shortcut expected-shortcut

win_class=$(xprop -id `xdotool getactivewindow` | ag -o '(?<=WM_CLASS\(STRING\) = ")[\w-]+(?=")')
if [ "$win_class" == "$1" ]; then
    xdotool key --delay 1 --clearmodifiers $3
else
    xdotool key --delay 1 --clearmodifiers $2
fi
    
   

