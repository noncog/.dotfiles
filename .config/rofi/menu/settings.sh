#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/settings.rasi"

# Options
desktop=" Desktop mode"
reloadi3=" Reload i3"
keybindsi3=" i3 Keybinds"
killemacs=" Kill Emacs"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$desktop\n$reloadi3\n$keybindsi3\n$killemacs"

chosen="$(echo -e "$options" | $rofi_command -p 'custom settings' -dmenu)"
case $chosen in
	$desktop)
		if [[ -f "$dir/desktop.sh" ]]; then
		    bash "$dir/desktop.sh"
		else
			msg "$desktop file not found"
		fi
        ;;
    $reloadi3)
		i3-msg reload
		i3-msg restart
        ;;
    $keybindsi3)
		grep -e '^[^#]*bind' ~/.config/i3/config | rofi -p 'keybinds' -dmenu
        ;;
	$killemacs)
		emacsclient -e "(kill-emacs)"
		killall emacs
        ;;
esac

#    $desktop)
#    $reloadi3)
#    $keybindsi3)
#    $killemacs)
