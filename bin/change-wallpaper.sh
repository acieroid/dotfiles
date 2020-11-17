#!/bin/sh
wget -O /home/quentin/.cache/wallpaper.jpg https://unsplash.it/2560/1440/?random
swaymsg output "*" background ~/.cache/wallpaper.jpg fill

