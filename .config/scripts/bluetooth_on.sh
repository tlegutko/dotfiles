#!/bin/sh
bluetoothctl << EOF
power on
quit
EOF
sleep 0.1
bluetoothctl << EOF
connect E5:27:BE:86:03:55
quit
EOF
