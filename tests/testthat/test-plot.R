movies = head(ggplot2movies::movies, 2000)
genres = c('Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance')


test_that("Simple plot works", {
    expect_warning(
        upset(movies, genres, min_size=100),
        regexp=NA
    )

    print(upset(movies, genres, min_size=100))
})


test_that("The example plot works", {
    library(ggplot2)

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


test_that("upset throws an informative error if filtering leads to empty dataset", {
    # intersection: size, degree
    # a:     1, 1
    # a-b:   2, 2
    # b-c:   1, 2
    # a-b-d: 1, 3
    # NA:    1, 0
    df = data.frame(
        a=c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
        b=c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
        c=c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
        d=c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
    )
    expect_error(
        upset(df, c('a', 'b', 'c', 'd'), min_size=3),
        'No intersections left after filtering: the maximal size for `min_size` for this dataset is 2'
    )
    expect_error(
        upset(df, c('a', 'b', 'c', 'd'), min_degree=4),
        'No intersections left after filtering: the maximal degree for `min_degree` for this dataset is 3'
    )
    expect_error(
        upset(df, c('a', 'b', 'c', 'd'), n_intersections=0),
        'No intersections left after filtering: provide `n_intersections` >= 1 \\(you provoided: 0\\)'
    )
    expect_error(
        upset(df, c('a', 'b', 'c', 'd'), max_size=-1),
        'No intersections left after filtering: provide `max_size` >= 1 \\(you provoided: -1\\)'
    )
    expect_error(
        upset(df, c('a', 'b', 'c', 'd'), max_degree=-1),
        'No intersections left after filtering: provide `max_degree` >= 0 \\(you provoided: -1\\)'
    )
})


test_that("upset_query requires set, group, or intersect, and not combinations & at least one aesthetic", {
    # requires at least on aesthetic
    expect_error(
        upset_query(set='Drama'),
        'Please pass at least one option or aesthetic \\(e.g. `color` or `fill`\\) to highlight the queried elements'
    )

    # accepts set
    expect_error(
        upset_query(set='Drama', fill='red'),
        NA
    )

    # accepts group
    expect_error(
        upset_query(group='Drama', fill='red'),
        NA
    )

    # accepts intersect
    expect_error(
        upset_query(intersect=c('Romance', 'Drama'), fill='red'),
        NA
    )

    # does not accept set and intersect together
    expect_error(
        upset_query(set='Drama', intersect=c('Romance', 'Drama'), fill='red'),
        'Please pass only one of: "set", "intersect", or "group"'
    )

    # requires are least set or intersect is present
    expect_error(
        upset_query(),
        'Please pass "set", "intersect", or "group"'
    )
})
