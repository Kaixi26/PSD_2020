#/bin/env bash

cd server_frontend
(echo "main:start_server(12345)." && cat) | ./start_erl.sh
