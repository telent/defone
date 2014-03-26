#!/bin/sh
export LD_LIBRARY_PATH=/usr/local/lib
export LEIN_ROOT=ok
lein trampoline run -m defone.nrepl
