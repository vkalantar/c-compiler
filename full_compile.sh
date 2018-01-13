#!/bin/bash
/mnt/c/Users/Varqa/Documents/Compiler/compiler $1
absolute_name=$(echo $1 | grep -oP ".*(?=\.c)")
gcc "$absolute_name.s" -o "$absolute_name"
rm "$absolute_name.s"