#!/usr/bin/env bash

_alarm() {
  ( \speaker-test --frequency $1 --test sine --scale 5 )&
  pid=$!
  \sleep 0.${2}s
  \kill -9 $pid
}
echo "start" >> /home/alx/screenshot.log

# paste current browser url to clipboard
browser_url=$(/home/alx/bin/get_url.py)
echo $browser_url >> /home/alx/screenshot.log
echo $browser_url | xclip -sel clip

_alarm 800 150
screenshot_root_path=/home/alx/org/inbox/screenshots/
screenshot_path="$screenshot_root_path"$(date +%Y-%m-%d_%H-%M-%S).png
flameshot full -p $screenshot_path

source /home/alx/org/.venv/bin/activate
python3 /home/alx/org/bin/process_inbox_screenshots.py -u $browser_url -s $screenshot_path 2&>1

_alarm 1000 200
