#' @importFrom stats as.formula kruskal.test p.adjust
NULL

#' Compare covariates between intersections
#'
#' @inheritParams upset_data
#' @param test the default test function; it is expected to accept `formula` and `data` parameters, and a list with `p.value`, `statistic`, and `method`
#' @param tests a named list with tests for specific variables, overwriting the default test
#' @param ignore a list with names of variables to exclude from testing
#' @param ignore_mode_columns whether the membership columns and size columns for all modes should be ignored
#' @param mode region selection mode; note that modes other than `exclusive_intersection` repeat observations in different test group, introducing dependencies. See `get_size_mode()` for accepted values.
#' @param ... passed to `upset_data()`
#' @export
compare_between_intersections = function(
    data, intersect, test=kruskal.test, tests=list(),
    ignore=list(), ignore_mode_columns=TRUE,
    mode='exclusive_intersection', ...
) {
  data = upset_data(data, intersect, mode=mode, ...)

  isect = data$with_sizes
  isect = isect[
      (isect[, paste0('in_', mode)] == 1) & (isect$intersection %in% data$plot_intersections_subset),
  ]

  modes = c(
      'exclusive_intersection',
      'inclusive_intersection',
      'exclusive_union',
      'inclusive_union'
  )

  if (ignore_mode_columns) {
      ignore = c(
          ignore,
          paste0('in_', modes),
          paste0(modes, '_size'),
          'exclusive_intersection'
      )
  }

  ignore = c('intersection', ignore)

  candidate_variables = setdiff(setdiff(names(isect), intersect), ignore)

  results = list()

  for (variable in candidate_variables) {

    if (variable %in% names(tests)) {
      var_test = tests[[variable]]
    } else {
      var_test = test
    }
    # TODO: there should be another test:
    #     variable ~ group_1 + group_2 + ... + group_n
    #  to assess "dose effect"

    # only needed for compatibility with 3.5.x
    # see: https://stackoverflow.com/q/18139195/6646912
    isect$intersection = as.factor(isect$intersection)

    result = var_test(as.formula(paste(variable, '~ intersection')), data=isect)

    results[[length(results) + 1]] = list(
      variable=variable,
      p.value=result$p.value,
      statistic=result$statistic,
      test=result$method
    )
  }

  if (length(results) == 0) {
    headers = c('variable', 'p.value', 'statistic', 'test', 'fdr')
    results = data.frame(matrix(
      ncol=length(headers),
      nrow=0,
      dimnames=list(NULL, headers)
    ))
  } else {
    results = do.call(rbind, lapply(results, data.frame))
    results$fdr = p.adjust(results$p.value, 'BH')
    rownames(results) = results$variable
  }

  results
}


#' Test for differences between intersections
#'
#' This is a wrapper around `compare_between_intersections()`, adding sorting by FDR, warnings, etc.
#' @inheritParams upset_data
#' @inheritDotParams compare_between_intersections
#' @export
upset_test = function(
    data,
    intersect,
    ...
) {
    comparison = do.call(compare_between_intersections, c(list(data, intersect), list(...)))

    if (nrow(comparison) == 0) {
        stop('No variables to compare')
    }

    significant_hits = comparison[comparison$fdr < 0.05, 'variable']

    if (length(significant_hits)) {
        print(paste(
          paste(significant_hits, collapse=', '),
          'differ significantly between intersections'
        ))
    }

    result = comparison[order(comparison$fdr), ]
    result
}
