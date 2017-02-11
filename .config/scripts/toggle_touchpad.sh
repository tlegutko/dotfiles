#!/bin/bash

CURR=$(xinput list-props "SynPS/2 Synaptics TouchPad" | grep "Device Enabled" | cut -f 3)
if [ $CURR -eq 0 ]; then 
  NEXT=1
  WS_POS=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).rect | .x+.width/2,.y+.height/2')
  xdotool mousemove ${WS_POS[0]} ${WS_POS[1]}
else 
  NEXT=0
fi
xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" $NEXT
