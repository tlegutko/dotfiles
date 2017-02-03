#!/bin/bash

CURR=$(xinput list-props "TPPS/2 IBM TrackPoint" | grep "Device Enabled" | cut -f 3)
if [ $CURR -eq 0 ]; then NEXT=1; else NEXT=0; fi
xinput set-prop "TPPS/2 IBM TrackPoint" "Device Enabled" $NEXT
