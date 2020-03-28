#!/usr/bin/env bash
set -e
echo "Compiling Python examples"
../scripts/expand_run_magic.py Examples.ipynb Examples_Python.ipynb
jupyter nbconvert --execute --to notebook --inplace Examples_Python.ipynb
echo "Compiling R examples"
../scripts/translate_examples.py Examples.ipynb _R_Examples.ipynb
../scripts/expand_run_magic.py _R_Examples.ipynb Examples_R.ipynb
jupyter nbconvert --execute --to notebook --inplace Examples_R.ipynb
echo "Done"
