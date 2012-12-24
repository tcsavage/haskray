#! /bin/sh
# Convert a folder of ppm images into pngs.

for f in *.ppm
do
	convert $f ${f%.*}.png
done
