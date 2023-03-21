#!/usr/bin/env bash

space_icons=(
    Emacs=""
    Safari=""
    kitty=""
    Anki="4"
)

#spaces_query="$(yabai -m query --spaces)"
spaces_query="$(yabai -m query --windows)"
for icon in "${space_icons[@]}"
do
    #space_match=$(echo "$spaces_query" | jq 'map(select(."label" == "'"${icon%=*}"'"))[0].index')
    space_match=$(echo "$spaces_query" | jq 'map(select(."app" == "'"${icon%=*}"'"))[0].space')

    if [[ ! "$space_match" == null ]]; then
        sketchybar --add space "${icon%=*}" left \
            --set "${icon%=*}" associated_space="$space_match" \
            icon="${icon#*=}" \
            background.color=0x44ffffff                \
            background.corner_radius=5                 \
            background.height=20                       \
            background.drawing=off                     \
            label.drawing=off                          \
            script="$PLUGIN_DIR/spaces/space.sh"       \
            click_script="yabai -m space --focus $space_match"
    else
        echo "found icon: $icon at: $space_match"
    fi
done
