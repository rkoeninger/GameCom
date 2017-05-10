#!/bin/sh

SDL_VERSION=2.0.3
FILE_NAME=SDL2-$SDL_VERSION.tar.gz

wget -q http://www.libsdl.org/release/$FILE_NAME
mkdir -p libsdl2
tar xf $FILE_NAME -C libsdl2
rm $FILE_NAME
cd libsdl2/SDL2-$SDL_VERSION
./configure
make
cd ..