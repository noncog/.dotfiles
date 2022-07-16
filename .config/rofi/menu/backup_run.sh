#!/usr/bin/bash

dir="$HOME/.config/rofi/menu"
rofi_command="rofi -theme $dir/configs/backup_run.rasi"

# set some key environment variables for borg
export BORG_REMOTE_PATH="/usr/local/bin/borg1/borg1"
export BORG_PASSCOMMAND="cat $HOME/.borg-passphrase"

info() {
	printf "%s %s\n" "$*" "$( date )" >&2;
}

backup() {
	info "Starting backup!"
    borg create --verbose --filter AME --list --stats --show-rc --compression lz4 zh2361@zh2361.rsync.net:borg-backups::'{hostname}-{now}' /home/jake/Documents
	
	backup_exit=$?

	info "Pruning repository!"
    borg prune --list --prefix '{hostname}-' --show-rc --keep-daily 7 --keep-weekly 4 --keep-monthly 6 --keep-yearly 3 zh2361@zh2361.rsync.net:borg-backups

	prune_exit=$?

	# use highest exit code as global exit code
	global_exit=$(( backup_exit > prune_exit ? backup_exit : prune_exit ))

	if [ ${global_exit} -eq 0 ]; then
		info "Backup and Prune finished successfully"
	elif [ ${global_exit} -eq 1 ]; then
		info "Backup and/or Prune finished with warnings"
	else
		info "Backup and/or Prune finished with errors"
	fi
	
	exit ${global_exit}
}

chosen="$(backup 2>&1 | $rofi_command -no-click-to-exit -p 'backups' -dmenu)"
