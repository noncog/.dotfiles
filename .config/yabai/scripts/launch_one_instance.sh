#!/usr/bin/env sh
# shellcheck disable=SC2046
if [ $(ps ax -c -o "command pid" | grep -c -E "^$1\s+[0-9]+$") -eq 0 ]; then
    $2
fi

# if [ $(pgrep -a "$1" | wc -l) -eq 0 ]; then
#     $2
# fi
