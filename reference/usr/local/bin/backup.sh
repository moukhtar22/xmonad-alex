cd /mnt/backup

if [ ! -f config ]; then
    echo 'Error, could not find the backup device'
    exit 1
fi


GETOPTS_OUT=$(getopt -o lua -l list,unmount,ask -n "backup.sh" -- "$@")
if [[ $? != 0 ]]; then
    echo "getopts gave erro; aborting" >&2
    exit 1
fi
eval set -- "$GETOPTS_OUT"
unset GETOPTS_OUT

ASK=0
BACKUP=1
LIST=0
UNMOUNT=0

while true; do
    case "$1" in
        -a | --ask )
            ASK=1
            shift ;;
        -l | --list )
            LIST=1
            shift ;;
        -u | --unmount )
            UNMOUNT=1
            shift ;;
        * ) shift; break ;;
    esac
done

if [[ $UNMOUNT -eq 1 ]]; then
    cd /mnt
    umount /mnt/backup
    echo "Unmounted Backup Device"
    exit
fi

if [[ $LIST -eq 0 ]]; then
    if [[ $BACKUP -eq 1 ]]; then
        if [[ $ASK -eq 1 ]]; then
            echo -n "Last update on: "
            sudo /usr/local/bin/backup.sh --list| tail -n1
            echo
            echo "Continue?"; read
        fi
        echo 'Starting Backup'
        borg create \
            --verbose --list --stats --progress \
            --exclude-caches --exclude 'home/*/.cache/*' --exclude 'root/.cache/*' \
            --exclude 'home/rosegrid/Qemu/*' \
            --exclude 'var/tmp/*' \
            --exclude 'var/db/repos/gentoo/*' --exclude 'var/db/repos/guru/*' \
            .::'{hostname}-{now}' \
            /etc /home /root /var /usr /nix /opt
        if [[ $? -ne 0 ]]; then
            su $SUDO_USER -c 'notify-send -t 3000 "Error: Backup Failed"'
            exit 1
        fi
        su $SUDO_USER -c 'notify-send -t 3000 "Backup Finished"'
    fi
else
    borg list .
fi
