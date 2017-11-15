#!/bin/sh


# usage ./remap-keyboard.sh 200 - sets the timeout for keypress
# if run without params, it sets delay to default 200ms
# if something goes wrong, control+m is treated as return in terminal / emacs

setxkbmap -layout pl -option numpad:microsoft -option ctrl:nocaps

# return as ctrl
xmodmap -e "remove Control = Control_R"
xmodmap -e "keycode 105 = Return"
xmodmap -e "keycode 36 = Control_R"
xmodmap -e "add Control = Control_R"

# both Alts are used as ISO_Level3_Shift(AltGr) for convenient polish characters typing
# Super and PrtSc next to Alts behave as Alt_L and Alt_R
xmodmap -e "clear Mod4"
xmodmap -e "clear Mod1"
xmodmap -e "keycode 133 = Alt_L"
xmodmap -e "keycode 107 = Alt_R"
xmodmap -e "keycode 64 = ISO_Level3_Shift"
xmodmap -e "add Mod1 = Alt_L Alt_R"

# Tab and backslash as additional Super keys
xmodmap -e "keycode 247 = backslash bar"
xmodmap -e "keycode 51 = Super_R"
xmodmap -e "keycode 248 = Tab"
xmodmap -e "keycode 23 = Super_L"
xmodmap -e "add Mod4 = Super_L Super_R"

# two extra Alts on grave / tilde and backspace
xmodmap -e "keycode 249 = grave asciitilde grave asciitilde notsign logicalor notsign"
xmodmap -e "keycode 49 = Alt_L"
xmodmap -e "keycode 250 = BackSpace BackSpace BackSpace BackSpace"
xmodmap -e "keycode 22 = Alt_R"

# kill running instance
curr_xcape=$(pgrep xcape)
[[ -n $curr_xcape ]] && kill $curr_xcape

timeout=150 # something default so I don't end up without return
if [ -n "$1" ]; then
    timeout=$1
fi

#when keys on the left are pressed within timeout threshold, they act as keys on the right
xcape -t $timeout -e "Control_L=Escape;Control_R=Return;Super_L=Tab;Super_R=backslash;Alt_L=grave;Alt_R=BackSpace"
