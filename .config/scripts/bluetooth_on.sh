#!/bin/sh
bluetoothctl << EOF
power on
quit
EOF
sleep 0.1
bluetoothctl << EOF
connect E4:22:A5:9C:23:28
quit
EOF
