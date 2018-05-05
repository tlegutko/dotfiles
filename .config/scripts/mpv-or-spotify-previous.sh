#!/bin/bash

if pgrep -x "mpv" > /dev/null
then
    echo '{"command": ["seek", "-5", "relative", "exact"] }' | socat - ~/.mpvsocket
else
    playerctl previous
fi
