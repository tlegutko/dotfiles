#!/bin/bash

if pgrep -x "mpv" > /dev/null
then
    echo '{"command": ["seek", "+10", "relative", "exact"] }' | socat - ~/.mpvsocket
else
    playerctl next
fi
