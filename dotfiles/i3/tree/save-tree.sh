#!/usr/bin/env bash

SCRIPT_PATH=`dirname -- "$0"`
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -o|--output-folder) SCRIPT_PATH="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

for i in $(seq 1 9);
do
    i3-save-tree --workspace $i > ${SCRIPT_PATH}/workspace-${i}.json
done
