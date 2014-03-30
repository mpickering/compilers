#!/bin/sh

for file in bin/*.p
do
  # do something on "$file"
  runghc Main.hs $file > a.x;
  ../compilers/lab2/ppc $file > b.x;
  echo $file;
  diff a.x b.x
done

