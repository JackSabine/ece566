#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

mkdir -p build
cd build
make

rm -rf p2-test
mkdir p2-test
cd p2-test

../../../../../wolfbench/configure --enable-customtool=$(readlink -f ../p2)
make all test
