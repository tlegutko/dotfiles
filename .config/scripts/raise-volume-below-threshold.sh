#!/bin/bash

pactl set-sink-mute @DEFAULT_SINK@ false \
  && (($(~/.config/scripts/sound_status.sh) <= 90)) \
  && pactl set-sink-volume @DEFAULT_SINK@ +10% \
  && $bar_refresh
