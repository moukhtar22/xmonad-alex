#!/bin/bash

# Try to run only one of these processes at a time
[[ "$(pgrep -x "soundctrl.sh")" != "$$" ]]&& exit 0

SINK="$(pactl list sinks| grep 'Sink #'| grep -o '[0-9]*')"
VOLUMENT_INCREMENT=5
CURRENT_VOLUME=
OPERATION="$1"


update_current_volume_status() {
    CURRENT_VOLUME=$(pactl get-sink-volume $SINK \
                         | awk -F',' 'NR==1 {print $1}' \
                         | grep -o "[0-9]*%" \
                         | tr -d ' ' \
                         | tr -d '%')
}

notify_at_checkpoint() {
    update_current_volume_status
    case $CURRENT_VOLUME in
        0) dunstify "Muted" --hints=int:value:0 --appname="soundctrl.sh"
           break ;;
        50) dunstify "Volume 50%" --hints=int:value:50 --appname="soundctrl.sh"
            break ;;
        100) dunstify "Volume 100%" --hints=int:value:100 --appname="soundctrl.sh"
             break ;;
    esac
}


update_current_volume_status

# These is no point in continuing if volume is already at 0 or 100.
# Ofcourse, the volume can technically be increased to more than a 100,
# but, I'm not sure what the actual max is (153% on my machine), and it sounds
# like too much trouble for a rare case to look into.
if [[ "$OPERATION" = "+" ]]; then
    [[ $CURRENT_VOLUME -ge 100 ]]&& exit 0
    pactl set-sink-volume $SINK $(( $CURRENT_VOLUME + $VOLUMENT_INCREMENT ))%
elif [[ "$OPERATION" = "-" ]]; then
    [[ $CURRENT_VOLUME -eq 0 ]]&& exit 0
    pactl set-sink-volume $SINK $(( $CURRENT_VOLUME - $VOLUMENT_INCREMENT ))%
fi

notify_at_checkpoint
