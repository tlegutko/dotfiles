#!/bin/sh
amixer | grep -Po '[0-9]+(?=%)' | head -1
