#!/bin/sh

terminal_num=$1
is_dropdown=$(xprop -id `xdotool getactivewindow` | grep -c "WM_WINDOW_ROLE(STRING) = \"${terminal_num}-dropdown-termite\"")
if ((is_dropdown == 1)); then
  i3-msg "[window_role="1-dropdown-termite"] move scratchpad"
  i3-msg "[window_role="2-dropdown-termite"] move scratchpad"
else
  ws=($(i3-msg -t get_workspaces | jq '.[] | select(.focused==true) | .rect | .x,.y,.width,.height/2 | floor'))
  i3-msg "[window_role="${terminal_num}-dropdown-termite"] scratchpad show; [window_role="${terminal_num}-dropdown-termite"] move position ${ws[0]} ${ws[1]}; resize set ${ws[2]} ${ws[3]}"
fi
