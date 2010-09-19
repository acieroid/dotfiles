#!/bin/sh
DEFAULTWM="/home/quentin/stumpwm/stumpwm"
#DEFAULTWM="twm"
urxvtd -q -f -o &
setxkbmap -option compose:menu
setxkbmap fr bepo
xset -b
xset r on
xmodmap ~/.xmodmaprc
if [ -n "$2" ]
then
  echo "Executing $2"
  exec $2
else
  echo "Executing default wm : $DEFAULTWM"
  exec $DEFAULTWM
fi
