#!/usr/bin/env bash

volume_slider=(
    script="$CONFIG_DIR/plugins/volume/volume.sh"
    updates=on
    label.drawing=off
    icon.drawing=off
    slider.highlight_color="$BLUE"
    slider.background.height=5
    slider.background.corner_radius=3
    slider.background.color="$BACKGROUND_2"
    slider.knob=􀀁
    slider.knob.drawing=off
)

volume_icon=(
    click_script="$CONFIG_DIR/plugins/volume/volume_click.sh"
    padding_right=10
    icon="$VOLUME_100"
    icon.width=0
    icon.align=left
    icon.color="$GREY"
    icon.font="$FONT:Regular:14.0"
    label.width=25
    label.align=left
    label.font="$FONT:Regular:14.0"
)

status_bracket=(
    background.color="$BACKGROUND_1"
    background.border_color="$BACKGROUND_2"
)

sketchybar --add slider volume e \
    --set volume "${volume_slider[@]}" \
    --subscribe volume volume_change \
    mouse.clicked \
    mouse.entered \
    mouse.exited \
    --add item volume_icon e \
    --set volume_icon "${volume_icon[@]}"

sketchybar --add bracket status brew volume_icon \
    --set status "${status_bracket[@]}"
