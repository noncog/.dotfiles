#!/usr/bin/env sh

scratchpad_space_id=$(yabai -m query --spaces | jq '.[] | select(.label=="scratchpad").index')
if [ -z "$scratchpad_space_id" ]; then
    space_count=$(yabai -m query --spaces | jq 'length')
    yabai -m space --create
    new_space_count=$(($space_count + 1))
    yabai -m space "$new_space_count" --label "scratchpad"
    printf "%s" "$new_space_count" > "$HOME"/.config/yabai/state/scratchpad_space
fi
