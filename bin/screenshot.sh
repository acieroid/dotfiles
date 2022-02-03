#!/bin/sh
GEOMETRY="$(slurp)"
sleep 2
grim -g "$GEOMETRY" /tmp/screenshot.png
