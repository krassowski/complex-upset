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


test_that("Data with hyphenated variables can queried", {

    df = data.frame(
        'a-x'=c(TRUE, FALSE, TRUE, TRUE),
        b=c(TRUE, TRUE, TRUE, TRUE),
        c=c(FALSE, TRUE, FALSE, FALSE),
        d=c(FALSE, FALSE, FALSE, TRUE),
        check.names=FALSE
    )

    expect_doppelganger(
        title='Hyphenated variables can be queried',
        upset(
            df,
            c('a-x', 'b', 'c', 'd'),
            queries=list(
                upset_query(set='a-x', fill='red'),
                upset_query(intersect=c('a-x', 'b'), fill='blue', color='blue')
            )
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


test_that("Filtering by degree in non-default mode with all intersections shows all observations", {
    abc_data = create_upset_abc_example()
    expect_doppelganger(
        title='Size for intersections=all, mode=union, and max_degree=1',
        upset(
            abc_data,
            colnames(abc_data),
            max_degree=1,
            mode='union',
            intersections='all'
        )
    )
})


test_that("Empty sets are removed during filtering with non-default mode", {
    expect_doppelganger(
        "Empty sets are removed when filtering non-default mode",
        (
            upset(
                movies, genres,
                n_intersections=5,
                mode='inclusive_intersection',
                keep_empty_groups=FALSE
            ) | upset(
                movies, genres,
                n_intersections=5,
                mode='inclusive_intersection',
                keep_empty_groups=TRUE
            )
        )
    )
})


test_that("Missing values are converted to FALSE", {
    # see https://github.com/krassowski/complex-upset/issues/88
    df = data.frame(
        set_a = c(1, 1, 1, NA),
        set_b = c(1, NA, NA, NA),
        set_c = c(NA, NA, 4, NA),
        set_d = c(NA, NA, NA, 1),
        category = c("E", "F", "F", "G")
    )

    expect_warning(
        upset(df, colnames(df)[1:2], min_degree=1),
        regexp='Detected missing values in the columns indicating sets, coercing to FALSE'
    )

    expect_doppelganger(
        "Degrees are filtered (min_degree=1) even if user used NA",
        upset(df, colnames(df)[1:2], min_degree=1)
    )
})


test_that("Sets of generated intersections are ok when sets not observed alone", {
    expect_doppelganger(
        "Sets with intersections='all' are ok when not observed",
        upset(
            # remove movies which belong to one set only
            movies[rowSums(movies[, genres]) != 1, ],
            genres,
            mode='inclusive_union',
            # generate all possible unions, enabling visualisation of unions with degree = 1
            intersections='all',
            keep_empty_groups=FALSE,
            # require unions of degree 1
            min_degree=1,
            max_degree=1
        )
    )
})


test_that("Inclusive union filtering works ok", {
    expect_doppelganger(
        "Inclusive union filtering works ok",
        upset(
            movies, genres,
            n_intersections=5,
            mode='exclusive_union',
            keep_empty_groups=FALSE
        )
    )
})


issue_101_reproduction_data = data.frame(matrix(data = TRUE, nrow=10, ncol=4))
issue_101_reproduction_data[10, 1] = FALSE
issue_101_reproduction_data[9, 2] = FALSE


test_that("Manually specified intersections that are not exclusive work ok", {

    expect_doppelganger(
        "Non-exclusive, manual intersections work with mode=inclusive",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2'),
            mode='inclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2')
            )
        )
    )

    expect_doppelganger(
        "Non-exclusive, manual intersections work with mode=inclusive with three sets",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2', 'X3'),
            mode='inclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2'),
                c('X1', 'X2', 'X3')
            )
        )
    )

    expect_doppelganger(
        "Non-exclusive, manual intersections work with mode=exclusive",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2'),
            mode='exclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2')
            )
        )
    )

    expect_doppelganger(
        "Non-exclusive, manual intersections show extra sets when included",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2', 'X3', 'X4'),
            mode='inclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2')
            )
        )
    )

    expect_doppelganger(
        "Mix of exclusive and non-exclusive manual intersections work with mode=inclusive",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2', 'X3', 'X4'),
            mode='inclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2'),
                c('X1', 'X2', 'X3', 'X4')
            )
        )
    )

    expect_doppelganger(
        "Mix of exclusive and non-exclusive manual intersections work with mode=exclusive",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2', 'X3', 'X4'),
            mode='exclusive_intersection',
            intersections=list(
                'X1',
                'X2',
                c('X1', 'X2'),
                c('X1', 'X2', 'X3', 'X4')
            )
        )
    )
})


test_that("Empty intersection can be included in manually specified intersections", {

    expect_doppelganger(
        "Empty intersection can be included in manually specified intersections",
        upset(
            issue_101_reproduction_data,
            c('X1', 'X2', 'X3', 'X4'),
            mode = "inclusive_intersection",
            intersections = list(
                'X1',
                'X2',
                c('X1', 'X2'),
                'Outside of known sets'
            )
        )
    )
})


test_that("Labels retain proper order when encoded", {
    data = data.frame(
        variable_2t = c(TRUE, FALSE, TRUE, FALSE),
        variable_1t = c(FALSE, FALSE, FALSE, TRUE),
        all_true = c(TRUE, TRUE, TRUE, TRUE)
    )

    expect_doppelganger(
        "Labels retain proper order when encoded (all_true has all dots)",
        upset(
            data,
            intersect = c("variable_1t", "variable_2t", "all_true"),
            encode_sets = T
        )
    )
})


test_that("Metadata gets properly reordered for Venn diagrams", {
    df = data.frame(
      name = 1:20,
      A = 1:20 %% 2 == 0,
      B = 1:20 %% 3 == 0,
      C = 1:20 %% 5 == 0
    )

    # this column is to track which rows have had TRUE in A
    df$WasA = df$A

    sets = c("A", "B", "C")
    arr = arrange_venn(df, sets)

    expect_doppelganger(
        "Metadata gets properly reordered for Venn diagrams",
        (
            ggplot(arr)
            + theme_void()
            + geom_venn_circle(data=df, sets=sets, size=1)
            + geom_venn_label_set(df, sets=sets, aes(label=region), outwards_adjust=2.6)
            + geom_point(aes(x=x, y=y, color=WasA), size=3)
        )
    )
})


test_that("Sets can be highlighted on intersection matrix", {
    expect_doppelganger(
        "Sets can be highlighted on intersection matrix",
        upset(
          movies,
          c('Action', 'Comedy', 'Drama'),
          min_size=10,
          width_ratio=0.2,
          queries=list(
            upset_query(set='Action', color='orange', fill='orange'),
            upset_query(set='Comedy', color='blue', fill='blue', only_components = 'overall_sizes'),
            upset_query(set='Drama', color='coral3', fill='coral3', only_components = 'intersections_matrix')
          )
        ) + labs(
            caption=paste(
                'Drama should be highlighted ONLY on intersection matrix',
                'Comedy should be highlighted ONLY on set sizes',
                'Action should be highlighted on BOTH',
                sep='\n'
            )
        )
    )
})


test_that("Color set by upset_query match", {

    all_genres = c(
        'Action', 'Animation', 'Comedy', 'Drama',
        'Documentary', 'Romance', 'Short'
    )

    expect_doppelganger(
        "Color of sets sizes bars matches: Drama=blue, Documentary=red",
        upset(
          data=head(movies, 80),
          intersect=all_genres,
          queries=list(
            upset_query(set="Drama", fill="blue"),
            upset_query(set="Documentary", fill="red")
          )
        )
    )

    expect_doppelganger(
        "Colors match when using multiple queries",
        upset(
            data=movies,
            intersect=all_genres,
            group_by="sets",
            width_ratio=0.1,
            queries=list(
                upset_query(group="Drama", color="blue"),
                upset_query(group="Comedy", color="brown1"),
                upset_query(group="Action", color="green"),
                upset_query(group="Romance", color="yellow"),
                upset_query(group="Animation", color="orange"),
                upset_query(group="Documentary", color="cyan"),
                upset_query(group="Short", color="darkorchid"),
                upset_query(set="Drama", fill="blue"),
                upset_query(set="Comedy", fill="brown1"),
                upset_query(set="Action", fill="green"),
                upset_query(set="Romance", fill="yellow"),
                upset_query(set="Animation", fill="orange"),
                upset_query(set="Documentary", fill="cyan"),
                upset_query(set="Short", fill="darkorchid"),
                upset_query(intersect="Drama", fill="blue"),
                upset_query(intersect="Comedy", fill="brown1"),
                upset_query(intersect="Action", fill="green"),
                upset_query(intersect="Romance", fill="yellow"),
                upset_query(intersect="Animation", fill="orange"),
                upset_query(intersect="Documentary", fill="cyan"),
                upset_query(intersect="Short", fill="darkorchid")
            )
        )
    )
})

