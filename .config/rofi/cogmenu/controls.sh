#!/usr/bin/bash

directory="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 57 -theme $directory/configs/cogmenu.rasi"

# options
desktop=" Desktop mode"
reloadi3=" Reload i3"
keybindsi3=" i3 Keybinds"
reloadxmodmap=" Reload Xmodmap"

# error msg
err_msg() {
    rofi -theme "$directory/configs/error.rasi" -e "$1"
}

# variables passed to rofi
options="$desktop\n$reloadi3\n$keybindsi3\n$reloadxmodmap"

selection="$(echo -e "$options" | $rofi_command -p 'controls' -dmenu)"
case $selection in
    $desktop)
    if [[ -f "$directory/desktop.sh" ]]; then
        bash "$directory/desktop.sh"
    else
        err_msg "$desktop file not found."
    fi
    ;;
    $reloadi3)
    i3-msg reload && i3-msg restart
    ;;
    $keybindsi3)
    grep -e '^[^#]*bind' ~/.config/i3/config | rofi -p 'i3 keybinds' -dmenu
    ;;
    $reloadxmodmap)
    xmodmap ~/.Xmodmap
    ;;
esac
