#!/bin/sh
if pgrep -a mpv | grep -q mpvsocket; then
    echo "mpv running"
else
    echo "mpv not running"
    mpv --input-ipc-server=/tmp/mpvsocket --video=no $(head -1 /home/quentin/.music)
fi
echo '{"command": ["cycle", "pause"]}' | socat - /tmp/mpvsocket
