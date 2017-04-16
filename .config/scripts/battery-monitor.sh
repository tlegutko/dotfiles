#!/bin/sh

battery_info=$(acpi -b)
battery_state=$(echo ${battery_info} | cut -d " " -f3)
battery_percentage=$(echo ${battery_info} | cut -d " " -f4 | grep -o "[0-9]\+")
if [[ "$battery_state" == "Discharging," ]] && [[ "$battery_percentage" -lt 20 ]]; then
    notify-send -u critical "Low battery alarm" "Just ${battery_percentage}% left, charge now!"
fi
  
