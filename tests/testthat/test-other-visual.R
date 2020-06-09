context('other-visual')
expect_doppelganger = vdiffr::expect_doppelganger


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


test_that("Multiple queries of the same kind highlight intersections", {
    movies = ggplot2movies::movies
    movies = movies[complete.cases(movies), ]
    genres = c('Action', 'Animation', 'Comedy', 'Drama', 'Documentary', 'Romance')

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
