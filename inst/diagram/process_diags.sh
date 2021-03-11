#!/usr/bin/env bash

blockdiag -Tsvg diag/lb.diag
blockdiag -Tsvg diag/lp.diag
blockdiag -Tsvg diag/hb.diag
blockdiag -Tsvg diag/hp.diag

if [ ! -e "out/diagrams" ]; then
   mkdir out/diagrams
fi

mv diag/*.svg out/diagrams/
