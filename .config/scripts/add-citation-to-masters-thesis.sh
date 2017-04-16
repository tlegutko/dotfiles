#!/bin/sh

printf "\n$(xsel -b)" >> ~/masters-thesis/masters-thesis.bib
~/.config/scripts/recompile-masters-thesis.sh

