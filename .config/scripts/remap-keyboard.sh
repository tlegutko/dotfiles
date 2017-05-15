#!/bin/sh

# caps lock and return act as control when pressed with another keys.
# when pressed alone, caps lock acts as escape

setxkbmap -layout pl -option ctrl:nocaps -option numpad:microsoft

xmodmap -e "remove Control = Control_R"
xmodmap -e "keycode 0x69 = Return"
xmodmap -e "keycode 0x24 = Control_R"
xmodmap -e "add Control = Control_R"

pgrep xcape | xargs kill
timeout=170 # something default so I don't end up without return
if [ -n "$1" ]; then
    timeout=$1
fi

xcape -t $timeout -e "Control_L=Escape;Control_R=Return"
