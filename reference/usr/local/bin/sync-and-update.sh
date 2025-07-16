#!/bin/bash

LOG_FILE="/var/log/gentoo_update.log"
MERGES_SORT_FILE="/var/cache/sorted_merges"

append_finish_date() {
    echo -e "==========================FIN: $(date)==========================\n\n" >> $LOG_FILE
}

echo "==========================BEG: $(date)==========================" >> $LOG_FILE

trap append_finish_date EXIT


notify() {
    # Expects 1 argument, the message
    if [ -n $SUDO_USER ]; then
        su $SUDO_USER -c "notify-send '$1'"
    else
        # For now, ignore it
        echo "$1"
    fi
}

sync_repos() {
    echo "eix-sync: $(date)>" >> $LOG_FILE
    eix-sync -q 2>>$LOG_FILE
    if [[ $? -ne 0 ]];then
        echo "Error encountered"
        exit 1
    fi
    echo "$(date) <eix-sync" >> $LOG_FILE
    echo "Syncing Complete"
}

warn_for_big_package() {
    # Expects 2 argument:
    #     package name
    #     test file
    if grep "$1" "$2" &>/dev/null; then
        notify "WARNING: one or more big packages are going to be emerged, maybe check the logs?"
        echo "WARNING: big packages" >> $LOG_FILE
    fi
}

export -f warn_for_big_package

pretend_packages_check() {
    # Expects 1 argument: emerge options
    echo "NOTE: will test for big packages"| tee -a $LOG_FILE
    [[ ! -f $MERGES_SORT_FILE ]]&& /usr/local/bin/sort_merges.sh
    PRETEND_LOG_FILE="/tmp/gentoo_update_pretend.log"
    emerge --pretend $1 @world &>$PRETEND_LOG_FILE
    cat $PRETEND_LOG_FILE| tee -a $LOG_FILE
    if [[ $(grep -c "sys-kernel/" $PRETEND_LOG_FILE) -ne 0 ]]; then
        # Autoupdating the kernel seems like a potential headache
        MESSAGE="ALERT: kernel Update Available, auto update aborted"
        notify $MESSAGE
        echo $MESSAGE| tee -a $LOG_FILE
        unset MESSAGE
        exit 1
    fi
    cat $MERGES_SORT_FILE| xargs -P$(nproc) -I% bash -c 'warn_for_big_package $0 $1' % "$PRETEND_LOG_FILE"
    echo "Checks Complete, will proceed to actual emerge"| tee -a $LOG_FILE
    unset PRETEND_LOG_FILE
}

update_world() {
    # Expects 1 argument: --ask(0|1)
    echo "Running @world update"
    echo "emerge -u @world> $(date)" >> $LOG_FILE
    EMERGE_UPDATE_OPTS="--update --changed-use --with-bdeps=y --deep"
    EMERGE_OPTS="--verbose --quiet $EMERGE_UPDATE_OPTS"
    if [[ "$1" -eq 1 ]]; then
        EMERGE_OPTS="--ask $EMERGE_OPTS"
    else
        # Notify about any potentially big packages
        pretend_packages_check "$EMERGE_UPDATE_OPTS"
        # Try to keep going even after a package gave an error
        EMERGE_OPTS="--keep-going $EMERGE_OPTS"
    fi
    emerge $EMERGE_OPTS @world 2>>$LOG_FILE
    if [[ $? -ne 0 ]];then
        # Notify about error if run as script
        [[ "$1" -eq 0 ]]&& notify "Emerge gave error while trying to update @world"
        echo "Error encountered"
        exit 1
    fi
    echo "$(date) <emerge -u @world" >> $LOG_FILE
    unset JOBS
}

clean_up() {
    # Expects 1 argument: --ask(0|1)
    echo "Cleaning up old sources/binaries"
    echo "cleaning> $(date)" >> $LOG_FILE
    if [[ $1 -eq 1 ]]; then
        emerge --depclean
    else
        echo "WARNING: no user prompt, emerge --depclean will be skipped"
    fi
    eclean-dist -d
    eclean-pkg -d
    echo "$(date) <cleaning" >> $LOG_FILE
}


GETOPTS_OUT=$(getopt -o SUCa -l no-sync,no-update,no-clean,ask -n "sync_and_update.sh" -- "$@")
if [[ $? != 0 ]]; then
    echo "getopts gave erro; aborting" >&2
    exit 1
fi
eval set -- "$GETOPTS_OUT"
unset GETOPTS_OUT

SYNC=1
UPDATE=1
CLEAN=1
ASK=0
while true; do
    case "$1" in
        -S | --no-sync )
            echo "WARNING: repositories will not be synced" >&2
            SYNC=0
            shift ;;
        -U | --no-update ) # Do not run emerge, implies --no-clean
            UPDATE=0
            CLEAN=0
            shift ;;
        -C | --no-clean ) # Do not clean the files
            echo "WARNING: files will not be cleared" >&2
            CLEAN=0
            shift ;;
        -a | --ask ) # Use --ask for emerge
            ASK=1
            shift ;;
        * ) shift; break ;;
    esac
done

if [[ $ASK -eq 0 ]]; then
    # Check for network reachability, give a 60s window
    MAX_TRIES=40
    TRIES=0
    while ! ping -c1 9.9.9.9 &>/dev/null; do
        if [[ $TRIES -lt $MAX_TRIES ]]; then
            TRIES=$(( $TRIES+1 ))
            echo "Network is unreachable, retrying"
            sleep 1.5
        else
            MESSAGE="Timed out while testing for network"
            notify "$MESSAGE"
            echo "$MESSAGE"| tee -a $LOG_FILE
            unset MESSAGE
            exit 1
        fi
    done
    unset MAX_TRIES
    unset TRIES
    # Do not proceed if there are unread news articles
    if [[ $(eselect news unread| wc -l) -ne 0 ]]; then
        notify "There are unread news articles, aborting"
        exit 1
    fi
fi

notify "Network: OK, proceeding with update"

[[ $SYNC -eq 1 ]]&& sync_repos
[[ $UPDATE -eq 1 ]]&& update_world $ASK
[[ $CLEAN -eq 1 ]]&& clean_up $ASK

unset SYNC
unset UPDATE
unset CLEAN
unset ASK
