#!/usr/bin/env bash

_alarm() {
  ( \speaker-test --frequency $1 --test sine --scale 5 )&
  pid=$!
  \sleep 0.${2}s
  \kill -9 $pid
}

_alarm 800 150
flameshot full -p /home/alx/org/inbox/screenshots/$(date +%Y-%m-%d_%H-%M-%S).png

source /home/alx/org/.venv/bin/activate
python3 /home/alx/org/bin/process_inbox_screenshots.py
_alarm 1000 200
