#!/usr/bin/bash

directory="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -location 2 -yoffset 57 -no-fixed-num-lines -theme $directory/configs/cogmenu.rasi"

# options
screenshot=" Screenshot"
brightness=" Brightness"
backups=" Backups"
controls=" Controls"
system=" System"

# error msg
err_msg() {
    rofi -theme "$directory/configs/error.rasi" -e "$1"
}

# variables passed to rofi
options="$screenshot\n$brightness\n$backups\n$controls\n$system"

selection="$(echo -e "$options" | $rofi_command -p 'cogmenu' -dmenu -selected-row 1)"
case $selection in
    $screenshot)
    if [[ -f "$directory/screenshot.sh" ]]; then
        bash "$directory/screenshot.sh"
    else
        err_msg "$screenshot file not found."
    fi
    ;;
    $brightness)
    if [[ -f "$directory/brightness.sh" ]]; then
        bash "$directory/brightness.sh" -selected-row 1
    else
        err_msg "$brightness file not found."
    fi
    ;;
    $backups)
    if [[ -f "$directory/rofi-borg/rofi-borg.sh" ]]; then
        bash "$directory/rofi-borg/rofi-borg.sh"
    else
        err_msg "$backups file not found."
    fi
    ;;
    $controls)
    if [[ -f "$directory/controls.sh" ]]; then
        bash "$directory/controls.sh"
    else
        err_msg "$controls file not found."
    fi
    ;;
    $system)
    if [[ -f "$directory/system.sh" ]]; then
        bash "$directory/system.sh"
    else
        err_msg "$system file not found."
    fi
    ;;
esac
