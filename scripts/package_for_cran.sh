rm -fr cran_package
mkdir cran_package
for folder in R man;
    do cp -r ${folder} cran_package/;
done;
mkdir cran_package/vignettes
cp -r vignettes/Examples_R_files cran_package/vignettes/
cp -r vignettes/Examples_R.Rmd cran_package/vignettes/
for file in DESCRIPTION LICENSE NAMESPACE README.md .Rbuildignore;
    do cp ${file} cran_package/;
done;
cd cran_package
../scripts/cmd_check.sh
../scripts/spell_check.sh
R CMD build .
