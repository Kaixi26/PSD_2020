#/bin/env bash
if [[ -z "$1" ]]; then
  echo "Please give request file as argument."
  exit
fi

IN=NC_IN.fifo
OUT=NC_OUT.fifo

rm $IN $OUT 2> /dev/null
mkfifo $IN
mkfifo $OUT

send(){
    (cat "$IN" | nc -c localhost 12345 | cat > $OUT) &
    
    exec 3>$IN
    cat $1 | grep -v "^#" | while read req;
    do
        echo "$req" > "$IN"
        echo "SENT $req"
        read rep < "$OUT"
        echo "RECV $rep"
    done
    exec 3>&-
}

send $1

rm $IN $OUT 2> /dev/null
