#/bin/sh
pushd diretorio
(mvn package &&
java -jar target/HelloWorld-1.0-SNAPSHOT.jar server config.yml)
