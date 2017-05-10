#!/bin/sh

SDL_VERSION=2.0.5
FILE_NAME=SDL2-$SDL_VERSION.tar.gz

wget -q http://www.libsdl.org/release/$FILE_NAME
mkdir -p libsdl2
tar xf $FILE_NAME -C libsdl2 --strip-components=1
rm $FILE_NAME
cd libsdl2
./configure
make
cd ..
