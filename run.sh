#!/bin/bash
/mnt/c/Users/Varqa/Documents/Compiler/compiler $1
echo "compiling complete"
absolute_name=$(echo $1 | grep -oP ".*(?=\.c)")
gcc "$absolute_name.s" -o "$absolute_name"
$absolute_name
echo $?
rm "$absolute_name"