#' @importFrom stats as.formula kruskal.test p.adjust
NULL

#' Compare covariates between intesections
#'
#' @export
compare_between_intersections = function(data, intersect, test=kruskal.test, tests=list(), ignore=list(), ...) {
  data = upset_data(data, intersect, ...)
  isect = data$intersected

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
    #     varaible ~ group_1 + group_2 + ... + group_n
    #  to assess "dose effect"
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
#' This is a wrapper around `compare_between_intersections()`, adding sorting by fdr, warnings, etc.
#' @export
upset_test = function(
    data,
    intersect,
    comparisons=list()
) {

    comparison = do.call(compare_between_intersections, c(list(data, intersect), comparisons))

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