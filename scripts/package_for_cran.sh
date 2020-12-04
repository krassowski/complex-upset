rm -fr cran_package
mkdir cran_package
for folder in R vignettes man;
    do cp -r ${folder} cran_package/;
done;
for file in DESCRIPTION LICENSE NAMESPACE README.md;
    do cp ${file} cran_package/;
done;
cd cran_package
../scripts/cmd_check.sh
../scripts/spell_check.sh
R CMD build
