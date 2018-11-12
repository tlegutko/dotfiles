#!/bin/sh

new_volume=$(($("/home/tlegutko/.config/scripts/sound_status.sh") + 10))
pactl set-sink-mute @DEFAULT_SINK@ false
pactl set-sink-volume @DEFAULT_SINK@ $((new_volume < 100 ? new_volume : 100))%
