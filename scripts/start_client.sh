#/bin/env bash

cd Client
#if [[ -z "$1" ]]; then
#    echo "No argument given."
#    read -n 1
#    exit
#fi
(mvn compile &&
mvn exec:java -q -Dexec.mainClass="main")
read -n 1
exit
