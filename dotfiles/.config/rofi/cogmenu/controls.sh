#!/usr/bin/bash

directory="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 57 -theme $directory/configs/cogmenu.rasi"

# options
onemonitor=" Primary Monitor"
twomonitor=" Two Monitors"
reloadi3=" Reload i3"
reloadxmodmap=" Reload Xmodmap"
keybindsi3=" i3 Keybinds"

# error msg
err_msg() {
    rofi -theme "$directory/configs/error.rasi" -e "$1"
}

# variables passed to rofi
options="$onemonitor\n$twomonitor\n$reloadi3\n$reloadxmodmap\n$keybindsi3"

selection="$(echo -e "$options" | $rofi_command -p 'controls' -dmenu)"
case $selection in
    $onemonitor)
    if [[ -f "$directory/primary-monitor.sh" ]]; then
        bash "$directory/primary-monitor.sh"
    else
        err_msg "$onemonitor file not found."
    fi
    ;;
    $twomonitor)
    if [[ -f "$directory/two-monitor.sh" ]]; then
        bash "$directory/two-monitor.sh"
    else
        err_msg "$twomonitor file not found."
    fi
    ;;
    $reloadi3)
    i3-msg reload && i3-msg restart
    ;;
    $reloadxmodmap)
    xmodmap ~/.Xmodmap
    ;;
    $keybindsi3)
    grep -e '^[^#]*bind' ~/.config/i3/config | rofi -p 'i3 keybinds' -dmenu
    ;;
esac
