#!/bin/bash

if [ `hostname -d` = "cs.brown.edu" ]; then
  rsync -rltovz --safe-links --delete build/ \
    flapjax@flapjax-vista:/home/flapjax/web/static
  rsync -rltovz --safe-links --delete persistence/ \
    flapjax@flapjax-vista:/home/flapjax/web/persistence
else
  echo "ERROR: you must deploy from a cs.brown.edu machine."
fi
