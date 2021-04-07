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


test_set_overlaps_of_degree = function(
    data, intersect, degree, remaining_degrees, diagnosis_rates, test, min_size
) {
    if (remaining_degrees == 0) {
        data_subset = data[diagnosis_rates$name]
        diagnosed_with_all = sum(apply(data_subset, 1, all), na.rm=TRUE)

        if (is.na(diagnosed_with_all)) {
            diagnosed_with_all = 0
        }

        if (diagnosed_with_all < min_size) {
            return(list())
        }

        having_data_for_all = sum(apply(!is.na(data_subset), 1, all), na.rm=TRUE)

        expected_overlap = prod(diagnosis_rates$rate)

        if (degree == 1) {
            # testing overlap does not make sense for degree=1 and could mislead FDR calculations
            p = list(
                statistic=NA,
                p.value=NA
            )
        } else {
            p = test(
                x=c(
                    'with_all_overlaping'=diagnosed_with_all,
                    'without_all_overlapping'=having_data_for_all - diagnosed_with_all
                ),
                p=c(
                    'with_all_overlapping'=expected_overlap,
                    'without_all_overlapping'=1 - expected_overlap
                )
            )
        }

        result = list(
            'expected_overlap'=expected_overlap,
            'expected_n'=expected_overlap * having_data_for_all,
            'observed_n'=diagnosed_with_all,
            'observed_overlap'=diagnosed_with_all / having_data_for_all,
            'sample_size'=having_data_for_all,
            'identifier'=paste(sanitize_names(diagnosis_rates$name), collapse='-'),
            'statistic'=p$statistic,
            'p_value'=p$p.value
        )
        return (result)
    } else {
        expected_overlaps = list()

        for (disease in intersect) {

            if (disease %in% diagnosis_rates$name) {
                next()
            }

            data_disease = data[[disease]]
            diagnoses = sum(data_disease, na.rm=TRUE)

            if (diagnoses < min_size) {
                next()
            }

            status_known_for = sum(!is.na(data_disease))
            diagnosis_rate = diagnoses / status_known_for

            diagnosis_rates_temp = rbind(
                diagnosis_rates,
                list(
                    'name'=disease,
                    'rate'=diagnosis_rate,
                    'diagnoses'=diagnoses,
                    'status_known_for'=status_known_for
                )
            )

            expected_overlap = test_set_overlaps_of_degree(
                data=data,
                intersect=intersect,
                diagnosis_rates=diagnosis_rates_temp,
                degree=degree,
                remaining_degrees=remaining_degrees - 1,
                test=test,
                min_size=min_size
            )

            if (length(expected_overlap)) {
                expected_overlaps[[length(expected_overlaps) + 1]] = expected_overlap
            }
        }
        return (data.frame(do.call(rbind, expected_overlaps)))
    }
}


test_set_overlaps = function(
    data, intersect,
    min_degre=1, max_degree=3,
    test=chisq.test,
    min_size=0,
    encode=FALSE,
    fdr_method='fdr'
) {
    data_subset = data[intersect]

    if (encode) {
        intersect = encode_names(intersect, avoid=c())
        colnames(data_subset) = intersect
    }

    all_expected_overlaps = list()

    for (degree in seq(min_degre, max_degree)) {

        expected_overlaps = test_set_overlaps_of_degree(
            data=data_subset,
            intersect=intersect,
            degree=degree,
            remaining_degrees=degree,
            diagnosis_rates=data.frame(name=character(), rate=numeric()),
            test=test,
            min_size=min_size
        )

        if (length(expected_overlaps)) {
            expected_overlaps$degree = degree
            all_expected_overlaps[[length(all_expected_overlaps) + 1]] = expected_overlaps
        }
    }

    df = data.frame(do.call(rbind, all_expected_overlaps))
    df = data.frame(lapply(df, unlist))
    df$enrichment = df$observed_overlap - df$expected_overlap
    df$phi = sqrt(df$statistic^2 / df$sample_size)
    df$fdr = p.adjust(df$p_value, method = fdr_method)
    df = df[order(-df$phi), ]
}
