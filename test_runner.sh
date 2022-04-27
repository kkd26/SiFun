#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

dune build

echo_green (){
    printf "${GREEN}$1${NC}\n"
}

echo_yellow (){
    printf "${YELLOW}$1${NC}\n"
}

echo_red (){
    printf "${RED}$1${NC}\n"
}

echo_line (){
    echo "-------------------------------------------------------------"
}

get_output (){
    output=$(dune exec -- sifun -e "$1")
    echo "$output"
}

get_type (){
    type=$(dune exec -- sifun -t -e "$1")
    echo "$type"
}

get_type_haskell (){
    # runghc -XRankNTypes -XScopedTypeVariables $1
    out=$(echo ":q" | ghci -XRankNTypes -XScopedTypeVariables -ghci-script $1)
    type=${out##*:: }
    type=${type%%Loaded*}
    echo "$type" | xargs
}

get_reduced (){
    reduced=$(dune exec -- sifun -e "$1")
    echo "${reduced#*|}" | xargs
}

run_test (){
    tempfile=${1/sf.in/tmp}
    start=`date +%s.%N`
    dune exec -- sifun -t $1 > $tempfile
    end=`date +%s.%N`
    echo "$end - $start" | bc -l
}

run_test_hs (){
    start=`date +%s.%N`
    get_type_haskell $1 > /dev/null
    end=`date +%s.%N`
    echo "$end - $start" | bc -l
}

compare_output (){
    tempfile=${1/sf.in/tmp}
    expected_file=${1/sf.in/out}
    haskell_file=${1/sf/hs}

    runtime=$(run_test $1)

    if [ -s $haskell_file ]; then
        runtime_hs=$(run_test_hs $haskell_file)
    else
        runtime_hs=""
    fi

    output=$(cat "$tempfile")
    expected=$(cat "$expected_file")

    echo -ne "${1##*/}\t"

    if ! [ -s $1 ]; then
        echo_yellow "EMPTY"
    elif [[ $expected == ":time" ]]; then
        echo_yellow "TIME_ONLY\t$runtime\t$runtime_hs"
    elif [[ $output == $expected ]]; then
        echo_green "OK\t\t$runtime\t$runtime_hs"
    else
        echo_red "ERROR\t\t$runtime\t$runtime_hs"
        diff --color $expected_file $tempfile
    fi

    rm $tempfile
}

compare_all (){
    files="$1/*.sf.in"
    for i in $files; do
        if [ -f "$i" ]; then
            compare_output $i
        fi
    done
}


get_test_dir (){
    if [ -z "$1" ]; then
        test_dir=$(dirname $0)
    else
        test_dir=$1
    fi
}

main (){
    get_test_dir $1
    echo -e "Running tests in $test_dir\n"
    echo -e "Name\t\tResult\t\tTime\t\tHaskell Time"
    echo_line
    compare_all $test_dir
    echo_line
}

main $1