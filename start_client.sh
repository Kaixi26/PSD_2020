#/bin/env bash

if [[ -n "$1" ]]; then
    T=$1
elif [[ -n "$TERM" ]]; then
    T=$TERM
else
    echo "TERM environment variable not found."
    echo "Please give your terminal emulator as parameter."
    exit
fi

echo "Using $T as terminal emulator"
$T -e ./scripts/start_client.sh
