#!/usr/bin/env bash
set -e
Rscript -e 'devtools::document()'
cd vignettes
../scripts/compile_examples.sh
cd ..
for v in Examples_Python Examples_R
do
    jupyter nbconvert  --to markdown "vignettes/${v}.ipynb" 
    mv "vignettes/${v}.md" "vignettes/${v}.Rmd"
    sed 's/```r/```{r eval=FALSE}/g' "vignettes/${v}.Rmd" -i
    sed 's/```python/```{python eval=FALSE}/g' "vignettes/${v}.Rmd" -i
    sed 's/\[png\]/[ ]/g' "vignettes/${v}.Rmd" -i
    sed -i "1s/^/---\ntitle: \"${v}\"\n---\n/" "vignettes/${v}.Rmd"
done
Rscript -e 'pkgdown::build_site()'
