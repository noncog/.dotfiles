#!/usr/bin/env sh

STATE_DIR="$HOME/.config/yabai/state"

# The specific 'x'_window_id file is not empty so we can do things.
if [ -s "$STATE_DIR"/"$1"_window_id ]; then

    win_id=$(cat "$STATE_DIR"/"$1"_window_id)
    focused_win=$(cat "$STATE_DIR"/"focused_window_id")
    scratchpad_space=$(cat "$STATE_DIR"/"scratchpad_space")
    focused_space=$(cat "$STATE_DIR"/"focused_space")

    # Change this algorithm to say that we must first check the space
    # We check if space = app_space
    # I now need to add individual space ids.
    if [ "$win_id" -eq "$focused_win" ]; then
        # If the window is currently focused, then we hide it.
        yabai -m window "$win_id" --space "$scratchpad_space"
        sketchybar --trigger windows_on_spaces
        . "$HOME/.config/yabai/scripts/update_scratchpad_space.sh"
    else
        # Otherwise we focus it.
        yabai -m window "$win_id" --space "$focused_space"
        sketchybar --trigger windows_on_spaces
        yabai -m window --focus "$win_id"
    fi
fi
