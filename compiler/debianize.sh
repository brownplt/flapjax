#!/bin/bash

DESTDIR="/usr/local"

./Setup.hs configure --user --prefix=$DESTDIR && \
./Setup.hs build && \
./Setup.hs copy --destdir=deb && \
chmod -R a+rX deb/usr && \
fakeroot dpkg -b deb fxc_2.0_i386.deb

	
