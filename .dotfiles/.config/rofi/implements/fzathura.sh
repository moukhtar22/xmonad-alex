#!/bin/bash

RECENTS_FILE="$HOME/.config/zathura/recents.log"
touch "$RECENTS_FILE"

PDF=
SAVE_RECENT=1

open_pdf() {
    echo "$PDF"
    if [[ -z "$PDF" ]]; then
	notify-send -t 500 "No pdf was specified"
	exit 1
    fi
    zathura --fork "$PDF"
    # Record opened file, unless requested otherwise
    [ $SAVE_RECENT -eq 1 ] && echo "$PDF" > $RECENTS_FILE
}

# Canonicalize the arguments
GETOPTS_OUT=$(getopt -o rmnp: -l resume,menu,no-save-recent,pdf: -n "fzathura-neo.sh" -- "$@")
if [[ $? != 0 ]]; then
    echo "getopts gave error, terminating" >&2
    exit 1
fi
eval set -- "$GETOPTS_OUT"

# Set the PDF to open
while true; do
    case "$1" in
	-r | --resume )
	    PDF="$(cat $RECENTS_FILE)"
	    [ -z $PDF ] && echo "recents file was empty"
	    shift
	    ;;
	-n | --no-save-recent )
	    echo "The recents file will not be overwritten"
	    SAVE_RECENT=0
	    shift
	    ;;
	-p | --pdf )
	    PDF="$2"
	    shift 2
	    ;;
	-m | --menu )
	    DOCUMENTS=""
	    [ -d $HOME/Documents ] || exit 1
	    cd $HOME/Documents
	    for DIR in *; do
		if [ -d $DIR ]; then
		    cd $DIR
		    for FILE in *.pdf; do
			DOCUMENTS=$DOCUMENTS"$DIR/$FILE\n"
		    done
		    cd ..
		fi
	    done
	    SELECTED_PDF="$(echo -e $DOCUMENTS| rofi -i -dmenu -theme $HOME/.config/rofi/themes/dmenu.rasi)"
	    echo $SELECTED_PDF
	    if [ -z "$SELECTED_PDF" ]; then
		echo "Nothing was selected"
		exit 1
	    fi
	    PDF="$HOME/Documents/$SELECTED_PDF"
	    shift
	    ;;
	* ) break ;;
    esac
done

# Now, open the pdf, if set
open_pdf
unset PDF
unset RECENTS_FILE
