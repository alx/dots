#!/usr/bin/env bash

_alarm() {
  ( \speaker-test --frequency $1 --test sine --scale 5 )&
  pid=$!
  \sleep 0.${2}s
  \kill -9 $pid
}

screenshot_root_path=/home/alx/org/inbox/screenshots/
screenshot_path="$screenshot_root_path"$(date +%Y-%m-%d_%H-%M-%S).png

browser_url=$(./get_url.py)

echo $screenshot_path
echo $browser_url

# paste current browser url to clipboard
echo $browser_url | xclip -sel clip

_alarm 800 150
flameshot full -p $screenshot_path

source /home/alx/org/.venv/bin/activate
python3 /home/alx/org/bin/process_inbox_screenshots.py \
  -u $(./get_url.py) \
  -s $screenshot_path

_alarm 1000 200
