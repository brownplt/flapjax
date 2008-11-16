#!/bin/bash
rsync -rltovz --safe-links --delete build/ \
  flapjax@flapjax.local:/home/flapjax/web/static
#rsync -rltovz --safe-links --delete persistence/ csadmin@plt2.cs.brown.edu:/home/csadmin/web/flapjax/persistence
#ssh csadmin@plt2.cs.brown.edu chmod -R a+rX /home/csadmin/web/flapjax
