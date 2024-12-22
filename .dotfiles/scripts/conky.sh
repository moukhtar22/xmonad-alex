#!/bin/bash

[ -z "$(pgrep conky)" ]|| killall conky

for config in ~/.config/conky/*.conf; do
    conky -c $config
done
