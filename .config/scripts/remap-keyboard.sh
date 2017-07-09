#!/bin/sh

# caps lock and return act as control when pressed with another keys.
# when pressed alone, caps lock acts as escape
# usage ./remap-keyboard.sh 200 - sets the timeout for keypress to be treated as ctrl
# if run without params, it sets delay to default 300ms
# if something goes wrong, control+m is treated as return in terminal / emacs

setxkbmap -layout pl -option numpad:microsoft -option ctrl:nocaps

# return as ctrl
xmodmap -e "remove Control = Control_R"
xmodmap -e "keycode 0x69 = Return"
xmodmap -e "keycode 0x24 = Control_R"
xmodmap -e "add Control = Control_R"

# Tab and backslash as additional Super keys
xmodmap -e "keycode 247 = backslash"
xmodmap -e "keycode 51 = Super_R"
xmodmap -e "keycode 248 = Tab"
xmodmap -e "keycode 23 = Super_L"

# Right alt behaves as left alt
xmodmap -e "clear Mod1"
xmodmap -e "keycode 108 = Alt_R"
xmodmap -e "add Mod1 = Alt_L Alt_R Meta_L"

# kill running instance
curr_xcape=$(pgrep xcape)
[[ -n $curr_xcape ]] && kill $curr_xcape

timeout=300 # something default so I don't end up without return
if [ -n "$1" ]; then
    timeout=$1
fi

xcape -t $timeout -e "Control_L=Escape;Control_R=Return;Super_L=Tab;Super_R=backslash"
