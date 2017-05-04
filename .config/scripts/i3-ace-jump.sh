#!/bin/bash

# Ace Jump over i3wm windows. Like EasyJump / Ace Jump / Avy, but for i3 windows, works on multiple displays.
# Paints a letter with Yad at upper left corner of each visible window and launches Yad to select a letter, then Control_L+Return selects that window. 
# dependencies & tools: i3 (duh), yad, xdotool, xwininfo, xprop

initially_active_window=$(xdotool getactivewindow)
vis_ws_nums=$(i3-msg -t get_workspaces | jq '.[] | select(.visible==true).num')
i3_ws_nums=$(xprop -root | grep -ioP "(?<=_net_desktop_names\(utf8_string\) =).*" | grep -oP "[0-9]+")
xprop_num=0
letters=("a" "s" "d" "f" "g" "q" "w" "e" "r" "t" "z" "x" "c" "v" "j" "k" "l" "m" "i" "o" "p")
letters_cnt=0
yad_entry_x=
yad_entry_y=
declare -A letter_to_window
for i3_num in $i3_ws_nums; do
    # xprop has different desktop names than i3, so those need to be found.
    if [[ $vis_ws_nums == *$i3_num* ]]; then
	win_ids=$(xdotool search --all --onlyvisible -desktop $xprop_num "" 2>/dev/null)
	for id in $win_ids; do
	    letter=${letters[$letter_cnt]}
	    letter_to_window+=([$letter]=$id)
	    letter_cnt=$(($letter_cnt+1))
	    win_pos=($(xwininfo -id $id | grep -ioP "(?<=Absolute upper-left [XY]:  )[0-9]+"))
	    win_pos_x=$((win_pos[0]>0 ? win_pos[0] : 1)) # 0 position doesn't work
	    win_pos_y=$((win_pos[1]>0 ? win_pos[1] : 1)) # 0 position doesn't work
	    if (( $initially_active_window != $id )); then
		echo ${letter} | yad --text-info --no-buttons --undecorated --borders=0  --no-focus --fontname="Monospace 30" --justify="center" --fore="#ff6000" --back="#3e3e3e" --posx=${win_pos_x} --posy=${win_pos_y} --on-top &
	    else
		yad_entry_x=$win_pos_x
		yad_entry_y=$win_pos_y
	    fi
	done
    fi
    xprop_num=$(($xprop_num+1))
done
selected_letter=$(yad --text-info --no-buttons --undecorated --borders=0 --fontname="Monospace 30" --justify="center" --fore="#ff6000" --back="#3e3e3e" --posx=${yad_entry_x} --posy=${yad_entry_y} --editable)
echo $selected_letter

# kill yad and don't spam output with termination messages
yads_to_kill=$(pgrep yad)
kill ${yads_to_kill}
wait ${yads_to_kill} 2>/dev/null

# change focus and move mouse to middle of new window
# if dmenu was cancelled, focus needs to return to initial window explicitly,
# because yad steals focus when killed
win_id=${letter_to_window[$selected_letter]-$initially_active_window}
win_x=$( xwininfo -id ${win_id} | grep -oP "(?<=Width: )[0-9]+" )
win_y=$( xwininfo -id ${win_id} | grep -oP "(?<=Height: )[0-9]+" )

#hide if it's a dropdown, window might be below it
is_dropdown=$(xprop -id ${initially_active_window} | grep -cP "^WM_NAME\(STRING\) = \"[0-9]-dropdown.*")
echo $is_dropdown
if ((is_dropdown == 1)); then
  echo "dropdown"
  i3-msg "[title="1-dropdown"] move scratchpad"
  i3-msg "[title="2-dropdown"] move scratchpad"
fi

i3-msg "[id=${win_id}] focus"
xdotool mousemove -w ${win_id} $(($win_x / 2)) $(($win_y / 2))
