#!/usr/bin/env bash
./scripts/prepare_for_tests.sh
Rscript -e 'devtools::test()'
