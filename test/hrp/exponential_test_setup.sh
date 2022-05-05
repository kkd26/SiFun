#!/bin/bash

haskell_pref="f = "
haskell_suf=":t f"
program0_sf="fn x => x"
program0_hs="\x -> x"

dir=$(dirname "$0")

init_files_sf (){
    touch $dir/T0{20..29}.sf.in
    touch $dir/T0{20..29}.out

    echo ":time" | tee $dir/T0{20..29}.out > /dev/null
}

make_next_sf (){
    echo "fn x => x ($1) ($1)"
}

make_next_hs (){
    echo "\x -> x ($1) ($1)"
}

make_n () {
    c=$2
    for i in $(seq 1 $3); do
        c=$($1 "$c")
    done
    echo "$c"
}

make_n_sf (){
    echo $(make_n make_next_sf "$program0_sf" $1)
}

make_n_hs (){
    echo $(make_n make_next_hs "$program0_hs" $1)
}

init_programs (){
    if [[ -n $HASKELL ]]; then
        pref=$haskell_pref
        suf=$haskell_suf
        make_fun=make_n_hs
        ext="hs.in"
    else
        make_fun=make_n_sf
        ext="sf.in"
    fi
    
    for i in {20..29}; do
        target="$dir/T0$i.$ext"
        num=$((i-20))
        program=$($make_fun $num)
        echo -n "$pref" > $target
        echo "$program" >> $target
        echo -n "$suf" >> $target
    done
}

while getopts "h" options; do
    case "${options}" in
        h)
            HASKELL=1
        ;;
    esac
done

main (){
    init_files_sf
    init_programs
}

main