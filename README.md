# ComplexUpset

[![DOI](https://zenodo.org/badge/236336935.svg)](https://zenodo.org/badge/latestdoi/236336935)

Quick links: [Documentation](https://krassowski.github.io/complex-upset/) | [Installation](https://github.com/krassowski/complex-upset#Install) | [R example](https://github.com/krassowski/complex-upset/blob/master/R_example.ipynb) | [Python example](https://krassowski.github.io/complex-upset/articles/Examples.html)

## Showcase

Get all the goodies of `UpSetR`, but with full extensibility of `ggplot2`. 

![Example UpSet plot](https://raw.githubusercontent.com/krassowski/complex-upset/master/movies.png)

<details>
    <summary>Click here to display the source code</summary>

```R
library(ggplot2)
library(ComplexUpset)

if(!require(ggplot2movies)) install.packages('ggplot2movies')
movies = ggplot2movies::movies
genres = c('Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance')

upset(
    movies,
    genres,
    annotations = list(
        'Length'=list(
            aes=aes(x=intersection, y=length),
            geom=geom_boxplot()
        ),
        'Rating'=list(
            aes=aes(x=intersection, y=rating),
            geom=list(
                # if you do not want to install ggbeeswarm, you can use geom_jitter
                ggbeeswarm::geom_quasirandom(aes(color=log10(votes))),
                geom_violin(width=1.1, alpha=0.5)
            )
        )
    ),
    queries=list(
        upset_query(
            intersect=c('Drama', 'Comedy'),
            color='red',
            fill='red',
            only_components=c('intersections_matrix', 'Intersection size')
        ),
        upset_query(
            set='Drama',
            fill='blue'
        ),
        upset_query(
            intersect=c('Romance', 'Drama'),
            fill='yellow',
            only_components=c('Length')
        )
    ),
    min_size=10,
    width_ratio=0.1
)
```

</details>


The full list of examples is available in the [documentation](https://krassowski.github.io/complex-upset/articles/Examples.html); it also contains instructions for the use from Python.

## Install

From `R` console run:

```R
if(!require(devtools)) install.packages("devtools")
devtools::install_github("krassowski/complex-upset")
```

## How it compares to other packags?

- [UpSetR](https://github.com/hms-dbmi/UpSetR) is a powerful tool and the pioneer in the UpSet visualisations; it was not designed to be extended with ggplot components. Unfortunately, the repository is no longer active (for two years now).
- [ggupset](https://github.com/const-ae/ggupset) uses scales to convert a single plot to upsets plots, e.g. with `scale_x_upset` - a really nice approach, recommended for simple ggplot.
- a cowplot-based upset was demonstrated in an [online publication](https://rpubs.com/alexeilutay/upsetr), however cowplot (a great tool at the time) got superceed by even greater tool: [pathwork](https://github.com/thomasp85/patchwork), which is what is used by ComplexUpset.

Use whichever tool you find the most useful for your particular use. The rational of making this repository public is not only to share the code, but also to demonstrate how simple it is now to create complex visualisations with patchwork (without the need to learn the ggplot internals).

For the interactive use, check out the [VCG/upset](https://github.com/VCG/upset). Regardless of the tool chosen, you may want to cite [(Lex et al, 2014)](https://dx.doi.org/10.1109/TVCG.2014.2346248) when using UpSet plots, especially in fields still dominated by Venn diagrams.

## Acknowledgements

Originally developed in course of a DPhil programme in Women's & Reproductive Health at [OxfordNuffieldWRH](https://github.com/OxfordNuffieldWRH).
