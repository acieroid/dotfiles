#!/bin/sh

if [ "$1" == "check" ]
then
    if [[ -f "$HOME/.pomodoro" ]]
    then
        START=$(cat "$HOME/.pomodoro")
        CUR=$(date +"%s")
        DIFF=$(echo "$CUR - $START" | bc)
        echo $DIFF > /tmp/diff.txt
        REMAINING=$(echo "25 * 60 - $DIFF" | bc)
        echo $REMAINING > /tmp/remaining.txt
        if [[ "$REMAINING" -lt 0 ]]
        then
           rm -f $HOME/.pomodoro
           notify-send 'Finished!' 'Timer finished.' --icon=dialog-information
        else
            echo '{"text": "' $(date +"%M:%S" -d "@$REMAINING") '", "class": "custom-pomodoro"}'
            exit
        fi
    fi
elif [ "$1" == "start" ]
then
    if [[ ! -f "$HOME/.pomodoro" ]]
    then
        date +"%s" > $HOME/.pomodoro
    fi
elif [ "$1" == "stop" ]
then
    rm -f $HOME/.pomodoro
else
   echo "Unknown command or no command provided"
   exit 1
fi

echo '{"text": "", "class": "custom-pomodoro"}'
