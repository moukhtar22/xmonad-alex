#!/bin/bash

if pgrep -x kanata &>/dev/null; then
    killall kanata
fi

kanata --nodelay --quiet --cfg ~/.config/kanata/kanata-alex.kbd &
kanata --nodelay --quiet --cfg ~/.config/kanata/kanata-alex.mouse &
