#!/bin/bash
for x in `ls *.jpg *.JPG`
do
	convert $x -resize 800x600 $x
	echo "done ${x}"
done
