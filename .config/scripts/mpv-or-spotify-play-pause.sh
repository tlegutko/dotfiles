#!/bin/bash

if pgrep -x "mpv" > /dev/null
then
    echo "cycle pause" | socat - ~/.mpvsocket
else
    playerctl play-pause
fi
