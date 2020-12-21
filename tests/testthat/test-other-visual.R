context('other-visual')
expect_doppelganger = vdiffr::expect_doppelganger
movies = ggplot2movies::movies
movies = movies[complete.cases(movies), ]
genres = c('Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance')


test_that("Data with hyphenated variables can be supplied", {

    df = data.frame(
        'a-x'=c(TRUE, FALSE, TRUE, TRUE),
        b=c(TRUE, TRUE, TRUE, TRUE),
        c=c(FALSE, TRUE, FALSE, FALSE),
        d=c(FALSE, FALSE, FALSE, TRUE),
        check.names=FALSE
    )

    expect_doppelganger(
        title='Hyphenated variables work as well',
        upset(
            df,
            c('a-x', 'b', 'c', 'd')
        )
    )
})


test_that("Columns names are available for plotting, even if not valid R variable names", {
    library(ggplot2)

    df = data.frame(
        'set a'=c(TRUE, FALSE, TRUE, TRUE),
        'set b'=c(TRUE, TRUE, TRUE, TRUE),
        'set c'=c(FALSE, TRUE, FALSE, FALSE),
        'set d'=c(FALSE, FALSE, FALSE, TRUE),
        check.names=FALSE
    )

    expect_doppelganger(
        title='Invalid columns names are available for plotting',
        upset(
            df, colnames(df),
            annotations=list(
                'Has members in set A?'=(
                    ggplot(mapping=aes(x=intersection, y=`set a`))
                    + geom_label(aes(label=`set a`))
                    + ylab('Has members in set A?')
                )
            )
        )
    )
})


test_that("Multiple queries of the same kind highlight intersections", {
    library(ggplot2)

    expect_doppelganger(
        title='Multiple queries of the same kind highlight intersections',
        upset(
            movies, genres, name='genre', width_ratio=0.1, min_size=100,
            queries=list(
                upset_query(
                    intersect=c('Drama', 'Comedy'),
                    color='red',
                    only_components=c('intersections_matrix')
                ),
                upset_query(
                  intersect=c('Romance', 'Drama'),
                  color='yellow',
                  only_components=c('intersections_matrix')
                )
            )
        )
    )
})


test_that("Multiple queries with different aes highlight intersections", {
    library(ggplot2)

    expect_doppelganger(
        title='Multiple queries with different aes highlight intersections',
        upset(
            movies, genres, name='genre', width_ratio=0.1, min_size=100,
            queries=list(
                upset_query(
                    intersect=c('Drama', 'Comedy'),
                    color='red',
                    fill='red',
                    only_components=c('intersections_matrix', 'Intersection size')
                ),
                upset_query(
                  intersect=c('Romance', 'Drama'),
                  color='yellow',
                  only_components=c('intersections_matrix')
                )
            )
        )
    )
})


test_that("Queries can highlight both intersection size and matrix", {
    library(ggplot2)

    expect_doppelganger(
        title='Queries can highlight both intersection size and matrix',
        upset(
            movies, genres, name='genre', width_ratio=0.1, min_size=100,
            queries=list(
                upset_query(
                    intersect=c('Drama', 'Comedy'),
                    color='red',
                    fill='red',
                    only_components=c('intersections_matrix', 'Intersection size')
                ),
                upset_query(
                  intersect=c('Romance', 'Drama'),
                  color='orange',
                  fill='orange',
                  only_components=c('intersections_matrix', 'Intersection size')
                )
            )
        )
    )
})


test_that("Multiple competing queries work together", {
    library(ggplot2)

    expect_doppelganger(
        title='Multiple competing queries work together',
        upset(
        movies, genres, name='genre', width_ratio=0.1, min_size=100,
        queries=list(
            upset_query(
                intersect=c('Drama', 'Comedy'),
                color='red',
                fill='red',
                only_components=c('intersections_matrix', 'Intersection size')
            ),
            upset_query(
              intersect=c('Romance', 'Drama'),
              color='orange',
              fill='orange',
              only_components=c('intersections_matrix', 'Intersection size')
            ),
            upset_query(
              intersect=c('Drama'),
              color='green',
              fill='green',
              only_components=c('intersections_matrix', 'Intersection size')
            ),
            upset_query(
              intersect=c('Comedy'),
              color='blue',
              fill='blue',
              only_components=c('intersections_matrix', 'Intersection size')
            )
        )
    )
    )
})


test_that("The empty intersection can be highlighted", {
    library(ggplot2)

    expect_doppelganger(
        title='The empty intersection can be highlighted',
        upset(
            movies, genres, name='genre', width_ratio=0.1, min_size=100,
            queries=list(
                upset_query(
                  intersect=NA,
                  color='green',
                  fill='green'
                )
            )
        )
    )
})


test_that("Counts are visible on top of highlighted bars", {
    library(ggplot2)

    expect_doppelganger(
        title='Counts are visible on top of highlighted bars',
        upset(
            movies, genres, name='genre', width_ratio=0.1, min_size=100,
            queries=list(
                upset_query(
                  intersect='Drama',
                  fill='blue'
                )
            )
        )
    )
})
