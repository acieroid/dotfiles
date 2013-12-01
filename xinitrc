#!/bin/sh
DEFAULTWM="/home/quentin/p/stumpwm/stumpwm"
#DEFAULTWM="openbox-session"
urxvtd -q -f -o &
xset -b
xset r on
xhost +
redshift -l 50.833:4.333 -m randr&
xmodmap /home/quentin/.Xmodmap
xbattbar -s ~/bin/xbattbar-check-acpi &
if [ -n "$2" ]
then
  echo "Executing $2"
  exec $2
else
  echo "Executing default wm : $DEFAULTWM"
  exec $DEFAULTWM
fi
