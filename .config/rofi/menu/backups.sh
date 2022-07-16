#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/backups.rasi"

# Options
backup=" Backup"
list=" List"
download=" Download"
delete=" Delete"

# Error msg
msg() {
	rofi -theme "$HOME/.config/rofi/menu/configs/message.rasi" -e "$1"
}

# Variable passed to rofi
options="$backup\n$list\n$download\n$delete"

chosen="$(echo -e "$options" | $rofi_command -p 'backups' -dmenu)"
case $chosen in
    $backup)
		if [[ -f "$dir/backup_run.sh" ]]; then
			dunstify "Backup" "Starting..."
			bash "$dir/backup_run.sh"
		else
			msg "$backup script not found."
		fi
        ;;
    $list)
		if [[ -f "$dir/backup_list.sh" ]]; then
			dunstify "Backup" "Listing..."
			bash "$dir/backup_list.sh"
		else
			msg "$list script not found."
		fi
        ;;
    $download)
		if [[ -f "$dir/backup_download.sh" ]]; then
		    bash "$dir/backup_download.sh"
		else
			msg "$download script not found."
		fi
        ;;
    $delete)
		if [[ -f "$dir/backup_delete.sh" ]]; then
			bash "$dir/backup_delete.sh"
		else
			msg "$delete script not found."
		fi
        ;;
esac


