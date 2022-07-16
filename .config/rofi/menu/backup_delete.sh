#!/usr/bin/bash

# rofi
dir="$HOME/.config/rofi/menu"
rofi_list="rofi -theme $dir/configs/backups_list.rasi"
rofi_delete="rofi -theme $dir/configs/backups_delete.rasi"

# set some key environment variables for borg
export BORG_REMOTE_PATH="/usr/local/bin/borg1/borg1"
export BORG_PASSCOMMAND="cat $HOME/.borg-passphrase"

# borg list command

list_backups() {
	chosen="$(borg list --format '{archive}{NL}' zh2361@zh2361.rsync.net:borg-backups | $rofi_list -no-click-to-exit -p 'delete backup' -dmenu)"
}

list_backups

borg delete zh2361@zh2361.rsync.net:borg-backups::"$chosen"

dunstify "Backup deleted:" "$chosen"



