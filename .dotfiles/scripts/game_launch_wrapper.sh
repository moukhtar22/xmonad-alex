#!/bin/bash

close_decors() {
	killall conky
	killall picom
}

enable_decors() {
	$HOME/scripts/conky.sh
	picom -b
}

trap enable_decors EXIT

# Now, kill all decorations, and run the game
close_decors
"$1"
