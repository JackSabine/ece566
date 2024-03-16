#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

cd build/p2-test

make all compare
