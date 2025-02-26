#!/bin/bash

if [[ -n "$(pgrep 'kmonad$')" ]]; then
    killall kmonad
fi


# Setup keyboard device
KBD_DEVICE="$1"
if [[ -z "$KBD_DEVICE" ]]; then
    KBD_DEVICE="$(find /dev/input/by-id/ -name '*kbd')"
fi

if [[ -n "$KBD_DEVICE" ]]; then
    echo "Using $KBD_DEVICE as keyboard device (making a copy of the previous config)"
    cp ~/.config/kmonad/kmonad-alex.kbd ~/.config/kmonad/kmonad-alex.kbd.old
    sed -i "s|/dev/input/by-id/[^\"]*|$KBD_DEVICE|" ~/.config/kmonad/kmonad-alex.kbd
else
    echo "No external keyboard device was found; aborting"
    echo "You might want to try specifying a device name manually"
    exit 1
fi

(~/.local/bin/kmonad ~/.config/kmonad/kmonad-alex.kbd &)
