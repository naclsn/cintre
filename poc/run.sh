#!/bin/sh
cd `git rev-parse --show-toplevel`/poc
set -ex
cc -Wall -Wextra -Wpedantic -c mylib.c -o mylib.o
cc -Wall -Wextra -Wpedantic adapt.c mylib.o -o adapt
exec ./adapt
