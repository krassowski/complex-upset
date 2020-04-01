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
