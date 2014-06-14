#!/bin/sh

mkdir pieces/small
cp pieces/*.png pieces/small
sips -Z 64 pieces/small/*.png