#!/usr/bin/env bash
set -e
sock_info=$(nc -W 1 -U /home/aidan/.cache/ncspot/ncspot.sock)

title=$(echo "$sock_info" | jq '.playable.title' | sed 's/\"//g')
artist=$(echo "$sock_info" | jq '.playable.artists[0]' | sed 's/\"//g')
song_length=$(echo "$sock_info" | jq '.playable.duration')
song_epoc=$(echo "$sock_info" | jq '.mode.Playing.secs_since_epoch')

if [ "$song_epoc" != null ]
then
  current_epoc=$(date +%s)
  current_secs=$((current_epoc-song_epoc))
else
  current_secs=$(echo "$sock_info" | jq '.mode.Paused.secs')
fi

time=$(date -d@$current_secs -u +%M:%S)
length=$(date -d@$((song_length/1000)) -u +%M:%S)

printf '%.30s - %.30s | %s / %s' "$artist" "$title" "$time" "$length"
