#!/bin/sh

terminal_num=$1
is_dropdown=$(xprop -id `xdotool getactivewindow` | grep -c "WM_NAME(STRING) = \"${terminal_num}-dropdown\"")
if ((is_dropdown == 1)); then
  i3-msg "[title="1-dropdown"] move scratchpad"
  i3-msg "[title="2-dropdown"] move scratchpad"
else
  ws=($(i3-msg -t get_workspaces | jq '.[] | select(.focused==true) | .rect | .x,.y,.width,.height/2 | floor'))
  i3-msg "[title="${terminal_num}-dropdown"] scratchpad show; [title="${terminal_num}-dropdown"] move position ${ws[0]} ${ws[1]}; resize set ${ws[2]} ${ws[3]}"
fi
