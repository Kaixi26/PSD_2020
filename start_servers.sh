#/bin/sh
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
(($T -e ./scripts/start_frontend.sh) &
echo "Starting frontend.")
(($T -e ./scripts/start_directory.sh) &
echo "Starting directory.")
(($T -e ./scripts/start_broker.sh) &
echo "Starting broker.")
echo "Press any key after servers have been initialized:"
read -n 1 key
for district in Braga Porto Lisboa; do
    (($T -e ./scripts/start_district.sh $district) &
     echo "Starting district '$district'.")
done
echo "All servers started."
