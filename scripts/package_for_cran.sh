rm -fr cran_package
mkdir cran_package
for folder in R man;
    do cp -r ${folder} cran_package/;
done;
mkdir cran_package/vignettes
mkdir cran_package/inst
cp -r vignettes/Examples_R_files cran_package/vignettes/
cp -r vignettes/Examples_R.Rmd cran_package/vignettes/
cp inst/CITATION cran_package/inst/
cp inst/WORDLIST cran_package/inst/
for file in DESCRIPTION NAMESPACE README.md .Rbuildignore;
    do cp ${file} cran_package/;
done;
# so CRAN requires a different "template" for MIT license...
# see: https://stackoverflow.com/questions/43550479/how-to-satisfy-both-cran-and-github-license-file-naming-requirements
# and: https://github.com/tidyverse/tidyr/issues/262
# and: https://r-pkgs.org/description.html#description-license
echo "YEAR: 2020
COPYRIGHT HOLDER: Michał Krassowski" > LICENSE
cd cran_package
../scripts/cmd_check.sh
../scripts/spell_check.sh
R CMD build .
R CMD check --as-cran ComplexUpset_*.tar.gz
