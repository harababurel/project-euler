#!/bin/bash

ignore_logs=0
while test $# -gt 0; do
    case "$1" in
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo -e "\t-i|--ignore-logs\tignore existing log files"
            echo -e "\t-h|--help\t\tdisplay this help and exit"
            exit 0
            ;;
        -i|--ignore-logs)
            echo "Will overwrite existing logs."
            ignore_logs=1
            shift
            ;;
        *)
            break
            ;;
    esac
done

# Compile and run all solutions that do not already have a log file.
# Write their execution time to main.log
ids=`find -name main.hs | grep -o "[0-9]\+" | sort -n`
for id in $ids; do

    if [ $ignore_logs == 0 ]; then
        find $id/main.log 2>/dev/null >/dev/null
        if [ $? == 0 ]; then
            echo -e "Problem $id already has a log."
            continue
        fi
    fi

    echo Compiling $id.
    ghc --make -O3 $id/main

    tmpfile=`mktemp`

    echo Running $id.
    find $id/input &> /dev/null
    if [ $? == 0 ]; then
        $( { time cat $id/input | $id/main; } 2>$tmpfile 1>/dev/null )
    else
        $( { time $id/main; } 2>$tmpfile 1>/dev/null )
    fi

    t=`cat $tmpfile | head -n 2 | tail -n 1 | cut -c 5-`
    echo $t > $id/main.log

    echo
done



echo "Problem | Execution time" > README.md
echo ":-------|:-------------:" >> README.md

withLogs=`find -name main.log | grep -o "[0-9]\+" | sort -n`
for id in $withLogs; do
    t=`cat $id/main.log`
    echo "$id | $t" >> README.md
done
