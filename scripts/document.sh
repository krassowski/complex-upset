#!/usr/bin/env bash
set -e
Rscript -e 'devtools::document()'
cd vignettes
../scripts/compile_examples.sh
cd ..
for v in Examples_Python Examples_R
do
    rm -rf "vignettes/${v}_files"
    jupyter nbconvert  --to markdown "vignettes/${v}.ipynb" 
    mv "vignettes/${v}.md" "vignettes/${v}.Rmd"
    sed 's/```r/```{r eval=FALSE}/g' "vignettes/${v}.Rmd" -i
    sed 's/```python/```{python eval=FALSE}/g' "vignettes/${v}.Rmd" -i
    sed 's/\[png\]/[ ]/g' "vignettes/${v}.Rmd" -i
    title=${v/_/ - }
    sed -i "1s/^/---\ntitle: \"${title}\"\nvignette: >\n  %\\\\VignetteEngine{knitr::rmarkdown}\n  %\\\\VignetteIndexEntry{${title}}\n  %\\\\usepackage[utf8]{inputenc}\n---\n\n\`\`\`{r echo=FALSE}\nknitr::opts_chunk\$set\(python.reticulate=FALSE\)\n\`\`\`\n\n/" "vignettes/${v}.Rmd"
done
rm -rf docs/articles
echo "Compressing images"
for v in Examples_Python Examples_R
do
    trimage -q -d "vignettes/${v}_files" || true
done
Rscript -e 'pkgdown::build_site()'
