#!/usr/bin/env sh

state_dir="$HOME/.config/yabai/state"

# The specific _window_id file is not empty so we can do things.
if [ -s "$state_dir"/"$1"_window_id ]; then

    win_id=$(cat "$state_dir"/"$1"_window_id)
    focused_win=$(cat "$state_dir"/"focused_window_id")
    scratchpad_space=$(cat "$state_dir"/"scratchpad_space")
    focused_space=$(cat "$state_dir"/"focused_space")

    if [ $win_id -eq $focused_win ]; then
        # If the window is currently focused, then we hide it.
        yabai -m window $win_id --space $scratchpad_space
    else
        # Otherwise we focus it.
        yabai -m window $win_id --space $focused_space
        yabai -m window --focus $win_id
    fi
fi
