context('examples')
expect_doppelganger = vdiffr::expect_doppelganger


test_that("The examples in documentation work as expected", {

    examples_notebook = jsonlite::fromJSON(system.file('Examples_R.ipynb', package='ComplexUpset'))
    cells = examples_notebook[['cells']]

    example_title = 'Example'
    last_title = example_title
    sub_examples_counter = 1

    for (cell_id in 1:nrow(cells)) {
        cell = cells[cell_id, ]
        cell_type = cell$cell_type

        if (!(cell_type %in% c('code', 'markdown'))) {
            print(paste('Unknown cell type:', cell_type))
            next()
        }
        lines = unlist(cell$source)

        if (cell_type == 'markdown') {

            for (line in lines) {
                if (startsWith(line, '#')) {
                    example_title = trimws(gsub('#+', '', line))
                }
            }

        } else if (cell_type == 'code') {
            code = paste(lines, collapse='\n')

            is_example = grepl('upset(', code, fixed=TRUE)

            if (is_example) {
                if (last_title == example_title) {
                    sub_examples_counter = sub_examples_counter + 1
                } else {
                    sub_examples_counter = 1
                }
                expect_doppelganger(
                    title=paste0('Example: ', example_title, ': ', sub_examples_counter),
                    eval(parse(text=code))
                )
                last_title = example_title
            } else {
                eval(parse(text=code))
            }
        }
    }
})
