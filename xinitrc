#!/bin/sh
DEFAULTWM="/home/quentin/p/dwm/dwm"
urxvtd -q -f -o &
xset -b
xset r on
xhost +
setxkbmap -option compose:menu
setxkbmap fr bepo
xmodmap ~/.xmodmaprc
redshift -l 50.833:4.333 -m vidmode&
~/p/dwmstatus/dwmstatus&
if [ -n "$2" ]
then
  echo "Executing $2"
  exec $2
else
  echo "Executing default wm : $DEFAULTWM"
  exec $DEFAULTWM
fi
