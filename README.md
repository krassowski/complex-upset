# ComplexUpset
A library for creating complex UpSet plots based on `ggplot2` and `patchwork`.

## Showcase

Get all the goodies of `UpSetR`, but with full extensibility of `ggplot2`. 

![Example UpSet plot](/movies.png)

```R
library(ggplot2)
library(ComplexUpset)


upset(
    movies, genres,
    annotations = list(
        'Length'=list(
            aes=aes(x=intersection, y=length),
            geom=geom_boxplot()
        ),
        'Rating'=list(
            aes=aes(x=intersection, y=rating),
            geom=list(
                # checkout ggbeeswarm::geom_quasirandom for better results!
                geom_jitter(aes(color=log10(votes))),
                geom_violin(width=1.1, alpha=0.5)
            )
        ),
        'Budget'=list(
            aes=aes(x=intersection, y=budget),
            geom=geom_boxplot()
        )
    ),
    min_size=10,
    width_ratio=0.1
)```


Click [here for more examples with the full source code here](Examples.ipynb). Also contains instructions for using from Python.

## Install

From `R` console run:

```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("krassowski/complex-upset")
```
