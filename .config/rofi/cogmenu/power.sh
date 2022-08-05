#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -theme $dir/configs/power.rasi"

# Options
power=" Power off"
restart=" Restart"
logout=" Logout i3"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/cogmenu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$power\n$restart\n$logout"

chosen="$(echo -e "$options" | $rofi_command -p 'system' -dmenu -selected-row 1)"
case $chosen in
    $power)
		systemctl poweroff
        ;;
    $restart)
		systemctl reboot
        ;;
    $logout)
		i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3?' -B 'Yes, exit i3' 'i3-msg exit'
        ;;
esac
