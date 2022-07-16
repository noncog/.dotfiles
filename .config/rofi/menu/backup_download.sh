#!/usr/bin/bash

# downloads
downloads_dir="$HOME/Downloads/borg-download"

# rofi
dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/backups_list.rasi"

# set some key environment variables for borg
export BORG_REMOTE_PATH="/usr/local/bin/borg1/borg1"
export BORG_PASSCOMMAND="cat $HOME/.borg-passphrase"

# borg list command
chosen="$(borg list --format '{archive}{NL}' zh2361@zh2361.rsync.net:borg-backups | $rofi_command -no-click-to-exit -p 'download backup' -dmenu)"

# make and move into downloads
mkdir $downloads_dir
cd "$downloads_dir"

# download backup
borg extract --list zh2361@zh2361.rsync.net:borg-backups::"$chosen"

dunstify "Downloaded backup:" "$chosen"
