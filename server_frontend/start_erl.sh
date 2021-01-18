#/bin/env bash
(cd deps; erlc *.erl)
erlc *.erl
erl -pz deps/ -pa deps/jiffy.beam
