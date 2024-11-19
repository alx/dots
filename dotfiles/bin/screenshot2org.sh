#!/usr/bin/env bash

_alarm() {
  ( \speaker-test --frequency $1 --test sine --scale 5 )&
  pid=$!
  \sleep 0.${2}s
  \kill -9 $pid
}

wm_output=$(wmctrl -lx | grep "Navigator.firefox")
window_title=$(echo "$wm_output" | sed -e 's/.*slim //' -e 's/ — Mozilla Firefox$//')

window_id=$(xdotool search --name "Mozilla" | head -n1)

echo $window_id
echo $window_title

sleep_duration=0.1
xdotool windowactivate "$window_id"
xdotool sleep $sleep_duration keydown Ctrl sleep $sleep_duration keydown l sleep $sleep_duration keyup Ctrl sleep $sleep_duration keyup l
xdotool sleep $sleep_duration keydown Ctrl sleep $sleep_duration keydown c sleep $sleep_duration keyup Ctrl sleep $sleep_duration keyup c
xdotool sleep $sleep_duration key Escape
browser_url=$(xsel -ob)

_alarm 800 050

screenshot_root_path=/home/alx/org/inbox/screenshots/
screenshot_path="$screenshot_root_path"$(date +%Y-%m-%d_%H-%M-%S).png
flameshot full -p $screenshot_path

_alarm 1600 050

echo $browser_url
echo $screenshot_path
echo $window_title

source /home/alx/org/.venv/bin/activate
python3 /home/alx/org/bin/process_inbox_screenshots.py \
  -u $browser_url \
  -s $screenshot_path \
  -t $window_title \
  2&>1

_alarm 1000 200
