#!/bin/bash

# get desired lines of output from i3's get_tree
lines=($(i3-msg -t get_tree | json_xs -t json-pretty | grep -A 30 '"floating" : "user_on"' | grep -e '"id"' -e '"output"' | cut -f2 -d":" | cut -f1 -d"," | tr -d '"' | tr -d ' ' | sed 'N;s/\n/=/'))

# if output is not '__i3', meaning is visible on a screen, then hide it.
for line in "${lines[@]}"; do
    key=${line%%=*}
    value=${line#*=}
    if [[ "$value" != "__i3" ]]; then
        i3-msg "[con_id="$key"] scratchpad show"
    fi
done
