test_that("The example plot works", {
    library(ggplot2)

    movies = ggplot2movies::movies
    genres = c('Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance')

    example_plot = function() {
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
                        geom_jitter(aes(color=log10(votes))),
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
    }

    expect_warning(
        example_plot(),
        regexp=NA
    )

    print(example_plot())
})