#!/usr/bin/env bash

#--- CONFIG ---
CLIP_READ="xclip -selection clipboard -o"
CLIP_WRITE="xclip -selection clipboard -i"
FONT="-fn mono-13"
BG="#222222"
FG="#eeeeee"

#--- ACTIONS ---
menu_items=(
  "base64"
  "base64 -d"
  "rot13"
  "jq -sRr @uri # urlencode"
  "jq -Rr @uri # urldecode"
  "wc -l"
  "wc -c"
  "wc -w"
  "xxd"
  "xxd -r"
  "xxd -p # hex encode"
  "xxd -p -r # hex decode"
)

show_menu() {
  printf "%s\n" "${menu_items[@]}" |
    dmenu -i -p "Clipboard action:"
}

in=$($CLIP_READ)
notify-send "Current clipboard" "$in"
action=$(show_menu)
[ -z "$action" ] && exit 0
action=$(printf "%s" "$action" | sed 's/# [^#]*$//')

out=$(printf "%s" "$in" | $action 2>&1)
printf "%s" "$out" | $CLIP_WRITE
notify-send "Clipboard transformed" "$out"

