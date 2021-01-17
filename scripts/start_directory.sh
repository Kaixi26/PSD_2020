#/bin/env bash

cd diretorio
(mvn package &&
java -jar target/HelloWorld-1.0-SNAPSHOT.jar server config.yml)
