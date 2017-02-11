#!/bin/bash

CURR=$(xinput list-props "SynPS/2 Synaptics TouchPad" | grep "Device Enabled" | cut -f 3)
if [ $CURR -eq 0 ]; then NEXT=1; else NEXT=0; fi
xinput set-prop "SynPS/2 Synaptics TouchPad" "Device Enabled" $NEXT
