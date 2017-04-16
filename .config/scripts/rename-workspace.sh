#!/bin/sh

curr_workspace_number=$(i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name' | grep -oP '(?<=^")[0-9]+')
new_name=$(printf "\n" | dmenu -b -p rename-workspace)
if [[ -n "${new_name}" ]]; then
    new_workspace_name="${curr_workspace_number}:${new_name}"
else
    new_workspace_name="${curr_workspace_number}"
fi
i3-msg "rename workspace to \"${new_workspace_name}\""
