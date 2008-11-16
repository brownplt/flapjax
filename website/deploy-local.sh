#!/bin/bash
rsync -rltovz --safe-links --delete build/ \
  flapjax@flapjax.local:/home/flapjax/web/static
rsync -rltovz --safe-links --delete persistence/ \
  flapjax@flapjax.local:/home/flapjax/web/persistence
