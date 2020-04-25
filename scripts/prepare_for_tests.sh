#!/usr/bin/env bash
cd vignettes
../scripts/translate_examples.py Examples.ipynb _R_Examples.ipynb
../scripts/expand_run_magic.py _R_Examples.ipynb Examples_R.ipynb
cp Examples_R.ipynb ../tests/testthat/Examples_R.json
cd ..
