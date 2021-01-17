#/bin/env sh
cd server_district
if [[ -z "$1" ]]; then
    echo "No argument given."
    read -n 1
    exit
fi
(mvn compile &&
mvn exec:java -q -Dexec.mainClass="Main" -Dexec.args="$1")
read -n 1
exit
