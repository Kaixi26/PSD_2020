#/bin/env bash

cd public_notifications_broker
(mvn compile &&
mvn exec:java -q -Dexec.mainClass="Broker")
read -n 1
exit