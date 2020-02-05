# ComplexUpset

A library for creating complex UpSet plots based on `ggplot2` and [`patchwork`](https://github.com/thomasp85/patchwork). Status: experimental.

[Examples](Examples.ipynb) | [Installation](https://github.com/krassowski/complex-upset#Install)

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
)
```


Click [here for more examples with the full source code here](Examples.ipynb). Also contains instructions for using from Python.

## Install

From `R` console run:

```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("krassowski/complex-upset")
```

## How it compares to other packags?

- [UpSetR](https://github.com/hms-dbmi/UpSetR) is a powerful tool and the pioneer in the UpSet visualisations; it uses base R graphic rather than ggplot.
- [ggupset](https://github.com/const-ae/ggupset) uses scales to convert a single plot to upsets plots, e.g. with `scale_x_upset` - a really nice approach, recommended for simple ggplot.
- a cowplot-based upset was demonstrated in an [online publication](https://rpubs.com/alexeilutay/upsetr), however cowplot (a great tool at the time) got superceed by even greater tool: [pathwork](https://github.com/thomasp85/patchwork), which is what is used by ComplexUpset.

Use whichever tool you find the most useful for your particular use. The rational of making this repository public is not only to share the code, but also to demonstrate how simple it is now to create complex visualisations with patchwork (without the need to learn the ggplot internals).

## Acknowledgements

Originally developed in course of a DPhil programme in Women's & Reproductive Health at [OxfordNuffieldWRH](https://github.com/OxfordNuffieldWRH).
