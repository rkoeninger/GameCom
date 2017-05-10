#!/bin/sh

SDL_VERSION=2.0.3
FILE_NAME=SDL2-$SDL_VERSION.tar.gz

wget -q http://www.libsdl.org/release/$FILE_NAME
tar xf $FILE_NAME -C libsdl2
rm 
cd libsdl2
./configure
make
cd ..
