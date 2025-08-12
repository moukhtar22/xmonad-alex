#!/bin/bash

#########################
WINDOW_MANAGER="~/.cache/xmonad/xmonad-x86_64-linux"

TOUCHPAD_ID=
ACCEL_PROP_ID=
TAP_PROP_ID=
TOUCHPAD_ACCELERATIION=0.55

REPEAT_DELAY=310
REPEAT_RATE=30

SCREEN_TIMEOUT=$(( 45 * 60 ))
SCREEN_LOCK_PROGRAM="xss-lock --transfer-sleep-lock -- ~/.local/bin/transfer-sleep-lock-generic-delay.sh --nofork"

#########################
if [ -f ~/.xinitrc ]; then
    mv ~/.xinitrc ~/old.xinitrc
fi

#########################
TOUCHPAD_ID="$(\
    xinput | grep -i 'touch' \
    | awk -F'=' '{print $2}' | awk '{print $1}' \
)"
ACCEL_PROP_ID="$(\
    xinput list-props $TOUCHPAD_ID | grep -i 'accel speed (' \
    | awk -F'(' '{print $2}' | awk -F')' '{print $1}' \
)"
TAP_PROP_ID="$(\
    xinput list-props $TOUCHPAD_ID | grep -i 'tapping enabled (' \
    | awk -F'(' '{print $2}' | awk -F')' '{print $1}' \
)"

#########################


if [[ -z $WINDOW_MANAGER ]] || [[ -z $TOUCHPAD_ID ]] || [[ -z $ACCEL_PROP_ID ]] \
       || [[ -z $TAP_PROP_ID ]] || [[ -z $TOUCHPAD_ACCELERATIION ]] || [[ -z $REPEAT_DELAY ]] \
       || [[ -z $REPEAT_RATE ]] || [[ -z $SCREEN_TIMEOUT ]] || [[ -z $SCREEN_LOCK_PROGRAM ]]; then
    echo "Error: Some variables could not be set, exiting"
    exit -1
fi
    

cat ~/xmonad-alex/helpers/dummy.xinitrc \
    | sed "s|%WINDOW_MANAGER%|$WINDOW_MANAGER|g" \
    | sed "s|%TOUCHPAD_ID%|$TOUCHPAD_ID|g" \
    | sed "s|%ACCEL_PROP_ID%|$ACCEL_PROP_ID|g" \
    | sed "s|%TAP_PROP_ID%|$TAP_PROP_ID|g" \
    | sed "s|%TOUCHPAD_ACCELERATIION%|$TOUCHPAD_ACCELERATIION|g" \
    | sed "s|%REPEAT_DELAY%|$REPEAT_DELAY|g" \
    | sed "s|%REPEAT_RATE%|$REPEAT_RATE|g" \
    | sed "s|%SCREEN_TIMEOUT%|$SCREEN_TIMEOUT|g" \
    | sed "s|%SCREEN_LOCK_PROGRAM%|$SCREEN_LOCK_PROGRAM|g" \
          > ~/.xinitrc
