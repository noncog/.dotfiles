#!/usr/bin/bash

dir="$HOME/.config/rofi/cogmenu"
rofi_command="rofi -no-fixed-num-lines -location 2 -yoffset 47 -theme $dir/configs/cogmenu.rasi"

# Options
screen=" Screen"
area=" Area"
window=" Window"

# Error msg
err_msg() {
    rofi -theme "$HOME/.config/rofi/cogmenu/configs/error.rasi" -e "$1"
}

# Variable passed to rofi
options="$screen\n$area\n$window"

chosen="$(echo -e "$options" | $rofi_command -p 'scrot' -dmenu)"
case $chosen in
    $screen)
	if [[ -f /usr/bin/scrot ]]; then
	    sleep 1; scrot -d 1 $HOME/pictures/screenshots/screen-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -d failed."
	fi
	;;
    $area)
	if [[ -f /usr/bin/scrot ]]; then
	    scrot -s $HOME/pictures/screenshots/area-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -s failed."
	fi
	;;
    $window)
	if [[ -f /usr/bin/scrot ]]; then
	    sleep 1; scrot -u $HOME/pictures/screenshots/window-%b%d::%H%M%S.png
	else
	    err_msg "Scrot -u failed."
	fi
	;;
esac
