# Kanata
~/scripts/kanata-init.sh

# Set up input devices
xinput set-prop %TOUCHPAD_ID% %ACCEL_PROP_ID% %TOUCHPAD_ACCELERATIION%
xinput set-prop %TOUCHPAD_ID% %TAP_PROP_ID% 1
xset r rate %REPEAT_DELAY% %REPEAT_RATE%

# Set screen saver timeout
xset s %SCREEN_TIMEOUT% %SCREEN_TIMEOUT%
xset dpms %SCREEN_TIMEOUT% %SCREEN_TIMEOUT% %SCREEN_TIMEOUT%

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Decoration
nitrogen --restore&
xsetroot -cursor_name left_ptr
xrdb ~/.Xresources
unclutter&

# Input Method
export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx

# Allow X server access to all local applications
xhost +si:localuser:$USER

# Screen Lock
%SCREEN_LOCK_PROGRAM% &

# Start WM
exec %WINDOW_MANAGER%
