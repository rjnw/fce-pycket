#!/usr/bin/zsh

for test_file in ./*rkt
do
    echo $test_file
    python ./target.py $test_file
done
