#!/bin/sh
HERE=$(dirname $0)
ROOT=$HERE/../..
GUILE_LOAD_PATH="$ROOT/demos:$ROOT/guile-modules" 
GUILE_AUTO_COMPILE="fresh"
LC_ALL="C.UTF-8"
$ROOT/tools/test-module.scm schess elements
