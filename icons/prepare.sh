#!/bin/sh

mkdir pieces/small
cp pieces/*.png pieces/small
sips -Z 64 pieces/small/*.png
for i in `ls pieces/small/*.png`; do
	convert $i -strip /tmp/$$-temp.png
	cp /tmp/$$-temp.png $i
done
