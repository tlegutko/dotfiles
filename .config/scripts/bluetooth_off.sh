#!/bin/sh
bluetoothctl << EOF
power off
quit
EOF
pulseaudio -k
pulseaudio --start

