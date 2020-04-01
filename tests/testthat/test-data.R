test_that("intersections names assembly", {
    expect_equal(
        apply(data.frame(a=c(TRUE, FALSE), b=c(TRUE, TRUE)), 1, names_of_true),
        c('a-b', 'b')
    )

    expect_equal(
        names_of_true(c(a=TRUE, b=TRUE)),
        'a-b'
    )

    expect_equal(
        names_of_true(c(`a-b`=TRUE, `x-y`=TRUE)),
        'a_b-x_y'
    )

    expect_error(
        names_of_true(c(`a-b`=TRUE, `a_b`=TRUE)),
        'The group names contain minus characters \\(-\\) which prevent intersections names composition.*'
    )
})


test_that("sort order parameter verification works", {
    expect_equal(
        check_sort('ascending'),
        TRUE
    )

    expect_equal(
        check_sort('descending'),
        TRUE
    )

    expect_equal(
        check_sort(FALSE),
        TRUE
    )

    expect_error(
        check_sort('abcd'),
        'Sort order has to be one of: descending or ascending, not "abcd"',
        fixed=TRUE
    )
})
