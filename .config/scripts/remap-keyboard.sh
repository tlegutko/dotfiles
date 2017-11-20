#!/bin/bash

# usage ./remap-keyboard.sh 200 - sets the timeout for keypress
# if run without params, it sets delay to default 200ms
# if something goes wrong, control+m is treated as return in terminal / emacs

setxkbmap -layout pl -option numpad:microsoft -option ctrl:nocaps

# return as ctrl
xmodmap -e "remove Control = Control_R"
xmodmap -e "keycode 105 = Return"
xmodmap -e "keycode 36 = Control_R"
xmodmap -e "add Control = Control_R"

# both physical Alts are used as ISO_Level3_Shift(AltGr) for convenient polish characters typing
# Super and PrtSc next to Alts behave as Alt_L and Alt_R
xmodmap -e "clear Mod4"
xmodmap -e "clear Mod1"
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 134 = Alt_R"
xmodmap -e "keycode 64 = ISO_Level3_Shift"

# Tab and backslash as additional Alt
xmodmap -e "keycode 247 = backslash bar"
xmodmap -e "keycode 51 = Alt_R"
xmodmap -e "keycode 248 = Tab"
xmodmap -e "keycode 23 = Alt_L"
xmodmap -e "add Mod1 = Alt_L Alt_R"

# two extra Alts on grave / tilde and backspace
xmodmap -e "keycode 249 = grave asciitilde grave asciitilde notsign logicalor notsign"
xmodmap -e "keycode 49 = Super_L"
xmodmap -e "keycode 250 = BackSpace BackSpace BackSpace BackSpace"
xmodmap -e "keycode 22 = Super_R"
xmodmap -e "add Mod4 = Super_L Super_R"

# kill running instance
curr_xcape=$(pgrep xcape)
[[ -n $curr_xcape ]] && kill $curr_xcape

timeout=300 # something default when called without arguments
if [ -n "$1" ]; then
    timeout=$1
fi

#when keys on the left are pressed within timeout threshold, they act as keys on the right
xcape -t $timeout -e "Control_L=Escape;Control_R=Return;Super_L=grave;Super_R=BackSpace;Alt_L=Tab;Alt_R=backslash"
