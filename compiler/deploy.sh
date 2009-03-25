#!/bin/bash

if [ `hostname -d` != "cs.brown.edu" ]; then
  echo "You must deploy from a cs.brown.edu machine."
  exit 1
fi;

DESTHOST="flapjax@fxinternal"
DESTDIR="/home/flapjax/compiler"

./Setup.hs configure --user --prefix=$DESTDIR && \
./Setup.hs build && \
./Setup.hs copy --destdir=dist/web && \
rsync -rltovz --copy-links --delete dist/web$DESTDIR/ $DESTHOST:$DESTDIR
	
