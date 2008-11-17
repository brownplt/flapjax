#!/bin/bash

if [ `hostname -d` = "cs.brown.edu" ]; then
  rsync -rltovz --safe-links --delete build/ \
    flapjax@flapjax-vista:/home/flapjax/web/static
else
  echo "ERROR: you must deploy from a cs.brown.edu machine."
fi
