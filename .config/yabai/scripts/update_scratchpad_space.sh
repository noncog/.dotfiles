#!/usr/bin/env sh

STATE_DIR="$HOME/.config/yabai/state"
# Get the real index of labeled scratchpad space.
scratchpad_space_id=$(yabai -m query --spaces | jq '.[] | select(.label=="scratchpad").index')

# Get the file which has the old space ID.
current_scratchpad_space=$(cat "$STATE_DIR"/"scratchpad_space")

# Get the space count
space_count=$(yabai -m query --spaces | jq 'length')

# Always move the scratchpad space label to the last space

if [ -z "$scratchpad_space_id" ]; then
    # Check if there is a labeled space.
    # If not, then create a new space and label it.
    # Then send the index to the state file.
    yabai -m space --create
    new_space_count=$(($space_count + 1))
    yabai -m space "$new_space_count" --label "scratchpad"
    printf "%s" "$new_space_count" > "$HOME"/.config/yabai/state/scratchpad_space
elif [ "$current_scratchpad_space" -ne "$space_count" ]; then
    yabai -m space "$space_count" --label "scratchpad"
    printf "%s" "$space_count" > "$HOME"/.config/yabai/state/scratchpad_space
fi
