#!/bin/bash

dir=$(dirname "$0")

touch $dir/T0{20..29}.in
touch $dir/T0{20..29}.out

echo ":time" | tee T0{20..29}.out > /dev/null

program0="fn x => x"

make_next (){
    echo "fn x => x ($1) ($1)"
}

make_n () {
    c=$program0
    for i in $(seq 1 $1); do
        c=$(make_next "$c")
    done
    echo "$c"
}

init_programs (){
    for i in {0..9}; do
        program=$(make_n $i)
        echo $program > "$dir/T02$i.in"
    done
}

init_programs