#!/usr/bin/bash

directory="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 57 -theme $directory/configs/cogmenu.rasi"

# options
up=" Up"
down=" Down"

# error msg
err_msg() {
    rofi -theme "$directory/configs/error.rasi" -e "$1"
}

# variables passed to rofi
options="$up\n$down"

selection="$(echo -e "$options" | $rofi_command -p 'brightness' -dmenu $1 $2)"
case $selection in
    $up)
    if [[ -f "$directory/brightness_change.sh" ]]; then
        bash "$directory/brightness_change.sh" + && bash "$directory/brightness.sh" -selected-row 0
    else
        err_msg "$up file not found."
    fi
    ;;
    $down)
    if [[ -f "$directory/brightness_change.sh" ]]; then
        bash "$directory/brightness_change.sh" - && bash "$directory/brightness.sh" -selected-row 1
    else
        err_msg "$down file not found."
    fi
    ;;
esac
