#!/bin/bash

# Canonicalize the arguments
GETOPTS_OUT=$(getopt -o ya -l yes,archive -n "backup.sh" -- "$@")
if [[ $? != 0 ]]; then
    echo "getopts gave error; aborting" >&2
    exit 1
fi
eval set -- "$GETOPTS_OUT"

RSYNCCMD="rsync -avxHAXP --numeric-ids --delete --dry-run"
MAKE_ARCHIVE="n"

# Setup variables
while true; do
    case "$1" in
	-y | --yes ) # Make the backup, no dry run
	    RSYNCCMD="rsync -avxHAXP --numeric-ids --delete"
	    shift
	    ;;
	-a | --archive ) # Also make a dated archive
	    echo "An lz4 compressed archive will be created"
	    MAKE_ARCHIVE="y"
	    shift
	    ;;
	* ) shift; break ;;
    esac
done


# Setup destination
DEST=$1
if [[ -z $DEST ]]; then
    echo "Backup destination not specified; aborting"
    exit 1
fi
cd "$DEST"

# Make the necessary backup directories
BACKUP_DIR="gentoo-lunaris"
[ ! -d $BACKUP_DIR ]&& mkdir -p $BACKUP_DIR

sudo $RSYNCCMD --exclude=/sys --exclude=/dev --exclude=/tmp --exclude=/proc --exclude=/boot \
               --exclude=/efi --exclude=/media --exclude=/home --exclude=/run --exclude=/mnt \
	       / $BACKUP_DIR

# Only selected /home files are to be backed up
for item in {Downloads,Documents,Pictures,Projects,Screenshots,.emacs.d,.opt,.local,.config,RPCS3,Authoring,Wallpapers,Qemu}; do
    $RSYNCCMD ~/$item $BACKUP_DIR/home/$(whoami)
done

# Make dated tar.zst archive
if [[ $MAKE_ARCHIVE = "y" ]]; then
    THIS_BACKUP_ARCHIVE="${BACKUP_DIR}_$(date '+%F').tar.lz4"
    sudo tar cvf - $BACKUP_DIR| lz4 -12 > $THIS_BACKUP_ARCHIVE
fi
