#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 47 -theme $dir/configs/cogmenu.rasi"

# options
power=" Power Off"
restart=" Restart"
logout=" Logout i3"

# variable passed to rofi
options="$power\n$restart\n$logout"

chosen="$(echo -e "$options" | $rofi_command -p 'system' -dmenu)"
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
