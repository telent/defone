#!/bin/sh
export LD_LIBRARY_PATH=/usr/local/lib
export LEIN_ROOT=ok
export LEIN_REPL_PORT=9990
lein repl
