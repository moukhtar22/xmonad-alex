#!/bin/bash

modprobe zram
modprobe lz4_compress
modprobe lz4_decompress

start_zram() {
    zramctl /dev/zram0 --algorithm lz4 --size 3.5GiB
    mkswap -U clear /dev/zram0
    swapon --discard --priority 100 /dev/zram0
}

KILL_ZRAM=0

GETOPTS_OUT=$(getopt -o k -l kill -n "initzram.sh" -- "$@")
[[ $? != 0 ]] && exit 1
eval set -- "$GETOPTS_OUT"

case "$1" in
    -k | --kill )
        if swapon | grep -q "zram0"; then
            KILL_ZRAM=1
        else
            echo "ZRam not running"
            exit 1
        fi
        shift ;;
    * ) shift; break ;;
esac

if [[ $KILL_ZRAM -eq 1 ]]; then
    swapoff /dev/zram0
    modprobe -r zram
else
    start_zram
fi
