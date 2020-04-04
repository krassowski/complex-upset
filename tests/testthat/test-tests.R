test_that("upset_test() does not fail", {
    df = data.frame(
        a=c(TRUE, FALSE),
        b=c(TRUE, TRUE),
        c=c(FALSE, TRUE),
        d=c(FALSE, FALSE),
        # x should be ignored
        x=c(2, 5)
    )

    result = upset_test(df, c('a', 'b', 'c', 'd'))

    expect_equal(
        colnames(result),
        c('variable', 'p.value', 'statistic', 'test', 'fdr')
    )

    expect_equal(
        as.character(result$variable),
        'x'
    )

    expect_equal(
        as.character(result$test),
        'Kruskal-Wallis rank sum test'
    )

    expect_equal(
        result$p.value,
        0.3173105
    )
})
