#!/bin/bash
HERE=$(dirname $0)
ROOT=$HERE/../..
GUILE_AUTO_COMPILE="fresh"
GUILE_LOAD_PATH="$ROOT/guile-modules" 
LC_ALL="C.UTF-8"

$ROOT/tools/ply23d.scm $HERE/data/cube.ply
if [ ! -f $HERE/data/cube.3d ]
    then echo "Failed to generate $HERE/data/cube.3d"
    exit 77
fi

$ROOT/tools/same-lisp.scm $HERE/data/cube.3d $HERE/data/cube.3d.norm
CODE=$?

rm $HERE/data/cube.3d
exit $CODE
