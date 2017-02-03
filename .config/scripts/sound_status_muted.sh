#!/bin/sh
amixer | grep -E '[0-9]*%' | head -1 | grep off > /dev/null
