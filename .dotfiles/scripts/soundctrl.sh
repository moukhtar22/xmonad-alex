#!/bin/bash

SINK="$(pactl list sinks| grep 'Sink #'| grep -o '[0-9]*')"
VOLUMENT_INCREMENT=5
CURRENT_VOlUME="$(pactl get-sink-volume 46| awk -F',' 'NR==1 {print $1}'| grep -o "[0-9]*%"| tr -d ' '| tr -d '%')"

OPERATION="$1"

if [[ "$OPERATION" = "+" ]]; then
    pactl set-sink-volume $SINK $(( $CURRENT_VOlUME + $VOLUMENT_INCREMENT ))%
elif [[ "$OPERATION" = "-" ]]; then
    pactl set-sink-volume $SINK $(( $CURRENT_VOlUME - $VOLUMENT_INCREMENT ))%
fi
