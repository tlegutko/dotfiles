#!/bin/sh

is_dropdown=$(xprop -id `xdotool getactivewindow` | grep -c "WM_WINDOW_ROLE(STRING) = \"dropdown-termite\"")
if ((is_dropdown == 1)); then
  i3-msg "move scratchpad"
else
  ws=($(i3-msg -t get_workspaces | jq '.[] | select(.focused==true) | .rect | .x,.y,.width,.height/2'))
  i3-msg "scratchpad show; [window_role="dropdown-termite"] move position ${ws[0]} ${ws[1]}; resize set ${ws[2]} ${ws[3]}; focus"
fi
