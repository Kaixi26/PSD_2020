#/bin/sh
erlc *.erl
erl -pz deps/ -pa deps/jiffy.beam
