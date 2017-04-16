#!/bin/sh

cd ~/masters-thesis
latex masters-thesis
bibtex masters-thesis
latex masters-thesis
pdflatex masters-thesis
