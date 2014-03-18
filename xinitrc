#!/bin/sh
#DEFAULTWM="xmonad"
DEFAULTWM="/home/quentin/p/stumpwm/stumpwm"
#DEFAULTWM="openbox-session"
which urxvtd 2>/dev/null
if [ $? -eq 0 ]; then
  urxvtd -q -f -o &
fi

which xset 2>/dev/null
if [ $? -eq 0 ]; then
  xset -b
  xset r on
fi


which xhost 2>/dev/null
if [ $? -eq 0 ]; then
  xhost +
fi


which redshift 2>/dev/null
if [ $? -eq 0 ]; then
  redshift -l 50.833:4.333 -m randr&
fi


which xmodmap 2>/dev/null
if [ $? -eq 0 ]; then
  xmodmap /home/quentin/.Xmodmap
fi

which xbattbar 2>/dev/null
if [ $? -eq 0 ]; then
  xbattbar -s ~/bin/xbattbar-check-acpi &
fi

if [ -n "$2" ]
then
  echo "Executing $2"
  exec $2
else
  echo "Executing default wm : $DEFAULTWM"
  exec $DEFAULTWM
fi
