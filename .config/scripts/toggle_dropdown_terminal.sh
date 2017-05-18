#!/bin/sh

terminal_num=$1
is_last_activated_dropdown=$(xprop -id `xdotool getactivewindow` | grep -c "WM_NAME(STRING) = \"${terminal_num}-dropdown\"")
if ((is_last_activated_dropdown == 1)); then
    for i in `seq 0 4`; do
	i3-msg "[title="${i}-dropdown"] move scratchpad"
    done    
else
  ws=($(i3-msg -t get_workspaces | jq '.[] | select(.focused==true) | .rect | .x,.y,.width,.height/2 | floor'))
  i3-msg "[title="${terminal_num}-dropdown"] scratchpad show; [title="${terminal_num}-dropdown"] move position ${ws[0]} ${ws[1]}; resize set ${ws[2]} ${ws[3]}"
fi
