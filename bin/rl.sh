#!/bin/sh
DIR="$(cd "$(dirname "$0")" && pwd)"
nohup "$DIR/rl-bin" "$@" >/dev/null 2>&1 &
