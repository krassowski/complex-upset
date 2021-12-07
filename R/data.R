#' @importFrom utils stack
NULL

NOT_IN_KNOWN_SETS = 'Outside of known sets'

sanitize_names = function(variables_names) {
    sanitized_names = c()
    for (name in variables_names) {
        if (grepl('-', name, fixed=TRUE)) {
            original_name = name
            name = gsub('-', '_', name)
            if (name %in% variables_names) {
            stop(paste(
                'The group names contain minus characters (-) which prevent intersections names composition;',
                'offending group:', original_name, 'please substitute these characters using gsub and try again.'
            ))
            }
        }
        sanitized_names = c(sanitized_names, name)
        }
    sanitized_names
}


encode_names = function(variables_names, avoid) {
    sapply(
        # using rank ensures that alphabetic order is retained in case of equal degrees
        # and that re-ordering columns in the dataframe will not lead to flickering of the result
        as.character(rank(variables_names)),
        function (name) {
            while (any(name %in% avoid)) {
                name = paste0(name, 'x')
            }
            name
        }
    )
}


names_of_members = function(row) {
  # the original implementation used which()
  #    members = names(which(row))
  # but which() is doing a few more things that are not needed;
  # it is an equivalent to seq_along(x)[!is.na(x) & x] + names assignment
  # and those steps can be omitted

  members = names(row)[row]
  if (length(members) != 0) {
      paste(members, collapse='-')
  } else {
      # this optimization may not be beneficial for dense matrices,
      # but we could add a heuristic that checks if the matrix is dense
      NOT_IN_KNOWN_SETS
  }
}


get_intersection_members = function(x) {
    strsplit(x, '-', fixed=TRUE)
}


gather = function(data, idvar, col_name, value_name='value') {
    not_idvar = colnames(data)
    not_idvar = not_idvar[not_idvar != idvar]
    result <- stack(data, select=not_idvar)
    result$group <- as.factor(rep(data[[idvar]], times=ncol(data) - 1))
    colnames(result) = c(value_name, col_name, idvar)
    result
}


compute_matrix = function(intersections_as_groups, sorted_groups) {

     matrix = sapply(
        intersections_as_groups,
        function(i_groups) {
            sorted_groups %in% i_groups
        },
        simplify=FALSE
    )

    matrix_data = as.data.frame(matrix, row.names=sorted_groups, check.names=FALSE)
    matrix_data
}



check_argument = function(
    value,
    allowed,
    description
) {
    if (!(value %in% allowed)) {
        stop(
            paste0(
                description,
                ' has to be one of: ',
                paste(allowed, collapse=' or '),
                ', not "',
                value,
                '"'

            )
        )
    }
}


check_sort = function(
    sort_order,
    allowed = c('descending', 'ascending'),
    what = 'order'
) {

    if (sort_order == FALSE) {
        return(TRUE)
    }

    check_argument(
        sort_order,
        allowed,
        paste('Sort', what)
    )

    TRUE
}


get_sort_order = function(data, sort_order) {
    check_sort(sort_order)

    if (sort_order == 'descending') {
        do.call(order, data)
    } else {
        do.call(order, lapply(data, function(x) {-x}))
    }
}


calculate_degree = function(x) {
    values = lengths(get_intersection_members(x))
    values[x == NOT_IN_KNOWN_SETS] = 0
    values
}


trim_intersections = function(
    intersections_by_size, min_size=0, max_size=Inf,
    min_degree=0, max_degree=Inf,
    n_intersections=NULL
) {
    intersections_by_size = intersections_by_size[
        (intersections_by_size >= min_size)
        &
        (intersections_by_size <= max_size)
    ]
    if (min_degree > 0 || max_degree != Inf) {
        degrees = calculate_degree(names(intersections_by_size))
        intersections_by_size = intersections_by_size[
            (degrees >= min_degree)
            &
            (degrees <= max_degree)
        ]
    }

    if (!is.null(n_intersections)) {
        intersections_by_size = tail(
            sort(intersections_by_size),
            n_intersections
        )
    }

    intersections_by_size
}


binary_grid = function(n, m) {
    if (m == 0) {
        return (matrix(rep(0, n), byrow=TRUE, nrow=1))
    }
    if (n == m) {
        return (matrix(rep(1, n), byrow=TRUE, nrow=1))
    }
    m_minus_n = m - n
    paths = list(
        c(0, rep(NA, n-1)),
        c(1, rep(NA, n-1))
    )
    sums = c(0, 1)
    for (level in 2:n) {
        upper_threshold = level + m_minus_n

        is_worth_adding_0 = (sums <= m) & (upper_threshold <= sums)
        is_worth_adding_1 = (sums <= m - 1) & (upper_threshold - 1 <= sums)

        x = paths[is_worth_adding_0]
        y = paths[is_worth_adding_1]

        for (i in 1:length(x)) {
            x[[i]][[level]] = 0
        }
        for (i in 1:length(y)) {
            y[[i]][[level]] = 1
        }
        paths = c(x, y)
        sums = c(sums[is_worth_adding_0], sums[is_worth_adding_1] + 1)
    }
    matrix(unlist(paths), byrow=TRUE, nrow=length(paths))
}

all_intersections_matrix = function(intersect, observed_intersections_matrix, min_degree, max_degree) {
    if (max_degree == Inf) {
        intersections_matrix = do.call(expand.grid, rep(list(0:1), length(intersect)))
    } else {
        if (max_degree > length(intersect)) {
            warning('provided `max_degree` was greater than the number of sets, reducing `max_degree` to the number of sets')
            max_degree = length(intersect)
        }
        intersections_matrix = do.call(rbind, lapply(min_degree:max_degree, function(degree) {
            binary_grid(n=length(intersect), m=degree)
        }))

        # need to add observed intersections too, otherwise the observations in intersections with other degrees would disappear
        # see https://github.com/krassowski/complex-upset/issues/89
        intersections_matrix = rbind(intersections_matrix, observed_intersections_matrix)
        intersections_matrix = intersections_matrix[!duplicated(intersections_matrix), ]
    }

    colnames(intersections_matrix) = intersect
    rownames(intersections_matrix) = apply(intersections_matrix == TRUE, 1, names_of_members)
    intersections_matrix = as.matrix(intersections_matrix)
    intersections_matrix
}


timer = NULL
profile = FALSE

note_time = function(text) {
    if (!profile) {
        return (NULL)
    }
    old = timer
    timer <<- Sys.time()
    if (!is.null(old))
        cat(paste(timer - old, text, '\n'))
}


intersection_vector_to_id = function (intersection_vector, sanitized_labels, sets_ordering_in_ids) {
    not_in_known_map = NOT_IN_KNOWN_SETS
    names(not_in_known_map) = NOT_IN_KNOWN_SETS
    sanitizer_map = c(sanitized_labels, not_in_known_map)
    sets = unname(sanitizer_map[intersection_vector])
    sets_ordering_in_ids = c(
        sets_ordering_in_ids,
        NOT_IN_KNOWN_SETS
    )
    paste(sets_ordering_in_ids[sets_ordering_in_ids %in% sets], collapse='-')
}


#' Prepare data for UpSet plots
#'
#' @param data a dataframe including binary columns representing membership in classes
#' @param intersect which columns should be used to compose the intersection
#' @param min_size minimal number of observations in an intersection for it to be included
#' @param max_size maximal number of observations in an intersection for it to be included
#' @param min_degree minimal degree of an intersection for it to be included
#' @param max_degree maximal degree of an intersection for it to be included
#' @param n_intersections the exact number of the intersections to be displayed; n largest intersections that meet the size and degree criteria will be shown
#' @param keep_empty_groups whether empty sets should be kept (including sets which are only empty after filtering by size)
#' @param warn_when_dropping_groups whether a warning should be issued when empty sets are being removed
#' @param warn_when_converting whether a warning should  be issued when input is not boolean
#' @param sort_sets whether to sort the rows in the intersection matrix (descending sort by default); one of: `'ascending'`, `'descending'`, `FALSE`
#' @param sort_intersections whether to sort the columns in the intersection matrix (descending sort by default); one of: `'ascending'`, `'descending'`, `FALSE`
#' @param sort_intersections_by the mode of sorting, the size of the intersection (cardinality) by default; one of: `'cardinality'`, `'degree'`, `'ratio'`, or any combination of these (e.g. `c('degree', 'cardinality')`)
#' @param sort_ratio_numerator the mode for numerator when sorting by ratio
#' @param sort_ratio_denominator the mode for denominator when sorting by ratio
#' @param group_by the mode of grouping intersections; one of: `'degree'`, `'sets'`
#' @param mode region selection mode for sorting and trimming by size. See `get_size_mode()` for accepted values.
#' @param size_columns_suffix suffix for the columns to store the sizes (adjust if conflicts with your data)
#' @param encode_sets whether set names (column in input data) should be encoded as numbers (set to TRUE to overcome R limitations of max 10 kB for variable names for datasets with huge numbers of sets); default TRUE for upset() and FALSE for upset_data()
#' @param intersections whether only the intersections present in data (`observed`, default), or all intersections (`all`) should be computed; using all intersections for a high number of sets is not computationally feasible - use `min_degree` and `max_degree` to narrow down the selection; this is only useful for modes different from the default exclusive intersection. You can also provide a list with a custom selection of intersections (order is respected when you set `sort_intersections=FALSE`)
#' @param max_combinations_datapoints_n a fail-safe limit preventing accidental use of `intersections='all'` with a high number of sets and observations

#' @export
upset_data = function(
    data, intersect, min_size=0, max_size=Inf, min_degree=0, max_degree=Inf,
    n_intersections=NULL,
    keep_empty_groups=FALSE,
    warn_when_dropping_groups=FALSE,
    warn_when_converting='auto',
    sort_sets='descending',
    sort_intersections='descending',
    sort_intersections_by='cardinality',
    sort_ratio_numerator='exclusive_intersection',
    sort_ratio_denominator='inclusive_union',
    group_by='degree',
    mode='exclusive_intersection',
    size_columns_suffix='_size',
    encode_sets=FALSE,
    # 10^10 fail-safe will allow for up to:
    # - for degree == 2: 500 sets x 100 observations, or 100 sets x 10 000 observations
    # - for degree <= 3: 150 sets x 100 observations, or 49 sets x 10 000 observations
    max_combinations_datapoints_n=10^10,
    intersections='observed'
) {
    # Check arguments
    mode = solve_mode(mode)

    if (length(intersections) == 1) {
        check_argument(
            intersections,
            allowed=c('observed', 'all'),
            description='intersections'
        )
        specific_intersections = FALSE
    } else {
        specific_intersections = TRUE
        if (!is.list(intersections)) {
            warning(paste0(
                '`intersections` is not `observed`, `all`, nor a list of vectors;',
                ' did you mean to use `list(c("A"), c("B"), c("A", "B"))`',
                ' instead of `c(c("A"), c("B"), c("A", "B"))`?'
            ))
        }
    }

    check_argument(
        group_by,
        allowed=c('degree', 'sets'),
        description='group_by'
    )

    check_sort(sort_sets)

    for (by in sort_intersections_by) {
        check_sort(by, allowed=c('cardinality', 'degree', 'ratio'), what='method')
    }

    intersect = unlist(intersect)

    if (specific_intersections) {
        sets_from_manual_intersections = setdiff(
            unique(unlist(intersections)),
            NOT_IN_KNOWN_SETS
        )
        sets_from_intersect = unique(intersect)
        missing_sets = setdiff(sets_from_manual_intersections, sets_from_intersect)
        if (length(missing_sets) != 0) {
            correct_missing_sets = base::intersect(
                colnames(data),
                missing_sets
            )
            incorrect_missing_sets = base::setdiff(
                missing_sets,
                colnames(data)
            )

            if (length(incorrect_missing_sets) != 0) {
                stop(
                    paste(
                        'Sets provided in `intersections` are missing in both `intersect` and in `data`:',
                        paste(incorrect_missing_sets, collapse=', ')
                    )
                )
            } else {
                warning(
                    paste(
                        'Following sets provided in `intersections` are missing in `intersect`:',
                        paste(missing_sets, collapse=', ')
                    )
                )
            }

            intersect = c(intersect, correct_missing_sets)
        }
    }
    if (length(intersect) == 1) {
        stop('Needs at least two indicator variables')
    }

    # Transform data
    note_time('initialised')

    if ('tbl' %in% class(data) | 'data.table' %in% class(data)) {
        data = as.data.frame(data)
    }

    # convert to logical if needed
    is_column_logical = sapply(data[, intersect], is.logical)
    if (any(!is_column_logical)) {
        non_logical = names(is_column_logical[is_column_logical == FALSE])

        if (warn_when_converting == 'auto') {
            unique_values = unique(
                as.vector(
                    as.matrix(
                        data[, non_logical]
                    )
                )
            )
            if (setequal(unique_values, c(0, 1))) {
                warn_when_converting = FALSE
            } else {
                warn_when_converting = TRUE
            }
        }
        if (warn_when_converting) {
            warning(paste('Converting non-logical columns to binary:', paste(non_logical, collapse=', ')))
        }

        data[, non_logical] = sapply(data[, non_logical], as.logical)
    }

    if (any(is.na(data[, intersect]))) {
        warning('Detected missing values in the columns indicating sets, coercing to FALSE')
        data[, intersect][is.na(data[, intersect])] = FALSE
    }

    intersect_in_order_of_data = colnames(data)[colnames(data) %in% intersect]

    non_sanitized_labels = intersect
    to_avoid = colnames(data)[!(colnames(data) %in% intersect)]

    if (encode_sets) {
        colnames(data)[colnames(data) %in% intersect] <- encode_names(intersect_in_order_of_data, avoid=to_avoid)
        intersect = unlist(encode_names(intersect, avoid=to_avoid))
    } else {
        colnames(data)[colnames(data) %in% intersect] <- sanitize_names(intersect_in_order_of_data)
        intersect = sanitize_names(intersect)
    }
    names(non_sanitized_labels) = intersect

    sanitized_labels = names(non_sanitized_labels)
    names(sanitized_labels) = non_sanitized_labels

    # sanitize or encode names of intersections selection/order
    if (specific_intersections) {
        intersections = sapply(intersections, function(intersection) {
            intersection_vector_to_id(
                intersection,
                sanitized_labels=sanitized_labels,
                sets_ordering_in_ids=intersect
            )
        })
    }

    note_time('converted data')

    data$intersection = apply(data[intersect], 1, names_of_members)

    unique_members_matrix = data[!duplicated(data$intersection), intersect]
    rownames(unique_members_matrix) = apply(unique_members_matrix, 1, names_of_members)

    # TODO: maybe use + to convert to numeric for speed (is it faster?)?
    unique_members_matrix = apply(unique_members_matrix, 1, as.numeric)

    observed_intersections_matrix = t(unique_members_matrix)

    if (specific_intersections) {

        if (mode == 'exclusive_intersection') {
            observed_intersections = rownames(observed_intersections_matrix)
            non_observed_exclusive_but_requested = setdiff(
                intersections,
                observed_intersections
            )

            translate_to_labels = function(endcoded_intersections) {
                sapply(
                    sapply(endcoded_intersections, get_intersection_members),
                    function(members) {
                        if (encode_sets) {
                            members = as.integer(members)
                        }
                        paste(non_sanitized_labels[members], collapse='-')
                    }
                )
            }

            if (length(non_observed_exclusive_but_requested) == length(intersections)) {
                non_observed_exclusive_but_requested_labels = translate_to_labels(
                    non_observed_exclusive_but_requested
                )
                observed_intersections_labels = translate_to_labels(
                    observed_intersections
                )
                warning(
                    paste0(
                        'None of the requested exclusive intersections is observed in the data:',
                        '\n  - requested: ',
                        paste(non_observed_exclusive_but_requested_labels, collapse =', '),
                        '\n  - available for exclusive intersection mode: ',
                        paste(observed_intersections_labels, collapse =', ')
                    )
                )
            }
        }

        # while this might seem strange to have duplicates, it would be a valid use case
        # e.g. to add a reference intersection multiple time for ease of comparison

        unique_intersections = unique(intersections)
        intersections_members = get_intersection_members(unique_intersections)

        sets_from_manual_intersections = setdiff(
            unique(unlist(intersections_members)),
            NOT_IN_KNOWN_SETS
        )

        # TODO: this is slow and memory hungry; ideally we would only get the relevant intersection straight away!
        possible_intersections = all_intersections_matrix(intersect, NULL, 0, Inf)

        relevant_intersections = rownames(possible_intersections[
            rowSums(possible_intersections[, sets_from_manual_intersections]) > 0,
        ])
        possible_intersections_members = get_intersection_members(relevant_intersections)

        # + to convert to numeric for consistency
        intersections_matrix = t(+sapply(
            possible_intersections_members,
            function(i) {
                intersect %in% i
            }
        ))
        colnames(intersections_matrix) = intersect
        rownames(intersections_matrix) = relevant_intersections

        unique_members_matrix = t(intersections_matrix)
        product_matrix = tcrossprod(intersections_matrix)
    } else if (intersections == 'observed') {
        intersections_matrix = observed_intersections_matrix
        colnames(intersections_matrix) = intersect
        product_matrix = intersections_matrix %*% unique_members_matrix
    } else if (intersections == 'all') {
        effective_max_degree = min(length(intersect), max_degree)

        combinations_n = sum(sapply(min_degree:effective_max_degree, function(m) choose(length(intersect), m)))
        datapoints_n = nrow(data) * ncol(data) * combinations_n

        if (datapoints_n > max_combinations_datapoints_n)  {
            degrees_text = ifelse(
                min_degree == max_degree,
                paste0(' equal ', min_degree),
                paste0('s between ', min_degree, ' and ', effective_max_degree)
            )

            advice_message = paste0(
                'The number of combinations with degree', degrees_text,
                ' (', formatC(combinations_n, format='e', digits=1), ') multiplied by the number of observations',
                ' (', nrow(data), ') and columns (', ncol(data), ') accounts to an upper bound of ',
                formatC(datapoints_n, format='e', digits=1), ' datapoints;',
                ' such a high number may lead to out of memory errors (depending on the available RAM size).',
                ' Please adjust `min_degree` and `max_degree`, remove unused columns, or',
                ' adjust `max_combinations_datapoints_n` (if you wish to proceed anyways).',
                '\nNote: filtering by size (`min_size` and/or `max_size`) or setting `n_intersections`',
                ' reduces the memory requirements and if you already do that',
                ' it may be safe to increase `max_combinations_datapoints_n`.'
            )
            stop(advice_message)
        }

        intersections_matrix = all_intersections_matrix(intersect, observed_intersections_matrix, min_degree, max_degree)
        unique_members_matrix = t(intersections_matrix)
        # note: tcrossprod is significantly faster than: intersections_matrix %*% unique_members_matrix
        product_matrix = tcrossprod(intersections_matrix)
    }
    note_time('calculated intersections')

    exclusive_intersection = table(data$intersection)
    observed_intersections = names(exclusive_intersection)
    exclusive_intersection = as.numeric(exclusive_intersection)
    names(exclusive_intersection) = observed_intersections

    product_matrix[product_matrix == 0] = -1

    if (NOT_IN_KNOWN_SETS %in% rownames(product_matrix) && NOT_IN_KNOWN_SETS %in% colnames(product_matrix)) {
        product_matrix[NOT_IN_KNOWN_SETS, ] = -1
        product_matrix[, NOT_IN_KNOWN_SETS] = -1
        product_matrix[NOT_IN_KNOWN_SETS, NOT_IN_KNOWN_SETS] = 0
    }

    exclusive_intersection_counts = exclusive_intersection[colnames(product_matrix)]
    inclusive_union = (product_matrix >= 0) * exclusive_intersection_counts

    observed_intersections_degrees = colSums(unique_members_matrix)
    desired_intersections_degrees = rowSums(intersections_matrix)

    exclusive_union = ((product_matrix >= 0) & (product_matrix >= observed_intersections_degrees)) * exclusive_intersection_counts

    if (NOT_IN_KNOWN_SETS %in% colnames(product_matrix)) {
        desired_intersections_degrees[NOT_IN_KNOWN_SETS] = 0
    }

    intersection_condition = t(t(product_matrix) >= desired_intersections_degrees)
    inclusive_intersection = intersection_condition * exclusive_intersection_counts

    if (!specific_intersections && intersections != 'observed') {
        exclusive_condition = t(t(product_matrix) == observed_intersections_degrees) & (product_matrix == observed_intersections_degrees)
        exclusive_intersection = exclusive_condition * exclusive_intersection_counts
        exclusive_intersection[is.na(exclusive_intersection)] = 0
        exclusive_intersection = colSums(exclusive_intersection)
    }
    note_time('calculated intersection sizes')

    inclusive_intersection[is.na(inclusive_intersection)] = 0
    exclusive_union[is.na(exclusive_union)] = 0
    inclusive_union[is.na(inclusive_union)] = 0

    sizes = list(
        exclusive_intersection=exclusive_intersection,
        inclusive_intersection=colSums(inclusive_intersection),
        exclusive_union=colSums(exclusive_union),
        inclusive_union=colSums(inclusive_union)
    )

    if (specific_intersections) {
        # add empty intersections if specified see:
        # - https://github.com/krassowski/complex-upset/issues/99
        # - https://github.com/krassowski/complex-upset/issues/104
        # - https://github.com/krassowski/complex-upset/issues/101
        for (kind in names(sizes)) {
            empty_intersections_to_include = setdiff(
                intersections,
                names(sizes[[kind]])
            )
            if (length(empty_intersections_to_include)) {
                sizes_of_empties = rep(0, length(empty_intersections_to_include))
                names(sizes_of_empties) = empty_intersections_to_include

                sizes[[kind]] = c(
                    sizes[[kind]],
                    sizes_of_empties
                )
            }
        }
    }

    intersections_by_size = sizes[[mode]]

    if (min_size > 0 || max_size != Inf || min_degree > 0 || max_degree != Inf || !is.null(n_intersections)) {
        intersections_by_size_trimmed = trim_intersections(
            intersections_by_size,
            min_size=min_size,
            max_size=max_size,
            min_degree=min_degree,
            max_degree=max_degree,
            n_intersections=n_intersections
        )
        if (length(intersections_by_size_trimmed) == 0) {

            if (min_size > 0) {
                tip = paste(': the maximal size for `min_size` for this dataset is', max(intersections_by_size))
            } else if (min_degree > 0) {
                degrees = calculate_degree(names(intersections_by_size))
                tip = paste(': the maximal degree for `min_degree` for this dataset is', max(degrees))
            } else if (!is.null(n_intersections) && n_intersections < 1) {
                tip = paste0(': provide `n_intersections` >= 1 (you provoided: ', n_intersections, ')')
            } else if (max_size < 1) {
                tip = paste0(': provide `max_size` >= 1 (you provoided: ', max_size, ')')
            } else if (max_degree < 0) {
                # note: max_degree = 0 returns observations that are not in any of the known sets
                tip = paste0(': provide `max_degree` >= 0 (you provoided: ', max_degree, ')')
            } else {
                tip = ''
            }

            stop(paste0('No intersections left after filtering', tip))
        }
    }

    if (min_size > 0 || max_size != Inf || !is.null(n_intersections)) {
        regions_to_include = colnames(inclusive_union)[
            colnames(inclusive_union) %in% names(intersections_by_size_trimmed)
        ]
    } else {
        regions_to_include = colnames(inclusive_union)
    }


    rownames(inclusive_union) = rownames(product_matrix)
    selected_intersections = intersect(colnames(inclusive_union), observed_intersections)

    original_data_indices = 1:nrow(data)
    indices_by_exclusive_intersection = split(original_data_indices, data$intersection)

    inclusive_union_indices = lapply(regions_to_include, function(region) {
        counts = inclusive_union[selected_intersections[selected_intersections != region], region]
        non_empty_subregions = names(counts[counts != 0])

        unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))
    })

    ## assert sapply(indices, length)) == colSums(inclusive_union[, union_to_be_added])

    lengths = sapply(inclusive_union_indices, length)
    all_indices = c(original_data_indices, unlist(inclusive_union_indices))
    offsets = cumsum(c(length(original_data_indices), lengths))
    names(offsets) = c(regions_to_include, NaN)


    # the initial length(original_data_indices) entries are only for regions of exclusive intersections
    # and indices here do not need any additional addressing offset. Following indices are for regions
    # that are not exclusive and require additional offest as follows:
    rownames(inclusive_intersection) = rownames(product_matrix)

    inclusive_intersections_counts = inclusive_intersection[
        intersect(colnames(inclusive_intersection), observed_intersections), , drop=FALSE
    ]
    names(inclusive_union_indices) = regions_to_include

    inlusive_intersection_ids = unlist(unname(sapply(regions_to_include, function(region) {
        counts = inclusive_intersections_counts[, region]
        non_empty_subregions = names(counts[counts != 0])

        indices_in_input_space = unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))

        additional_indices = which(inclusive_union_indices[[region]] %in% indices_in_input_space)
        offsets[[region]] + additional_indices
    })))

    rownames(exclusive_union) = rownames(product_matrix)

    exclusive_intersections_counts = exclusive_union[
        intersect(colnames(exclusive_union), observed_intersections), , drop=FALSE
    ]

    exclusive_union_ids = unlist(unname(sapply(regions_to_include, function(region) {
        counts = exclusive_intersections_counts[, region]
        non_empty_subregions = names(counts[counts != 0])

        indices_in_input_space = unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))

        additional_indices = which(inclusive_union_indices[[region]] %in% indices_in_input_space)
        offsets[[region]] + additional_indices
    })))


    data = data[all_indices, ]
    data$exclusive_intersection = data$intersection[all_indices]

    data$intersection = c(
        data$intersection[original_data_indices],
        rep(regions_to_include, times=lengths)
    )
    exclusive_intersection_indices = original_data_indices
    data$in_exclusive_intersection = c(
        rep(c(1, 0), times=c(length(exclusive_intersection_indices), sum(lengths)))
    )
    data$in_inclusive_union = 1

    data[, 'in_inclusive_intersection'] = data$in_exclusive_intersection
    data[inlusive_intersection_ids, 'in_inclusive_intersection'] = 1
    # note: new_indices = 1:nrow(data); new_indices %in% all_inlusive_intersection_ids is slightly slower
    # assuming all_inlusive_intersection_ids = c(exclusive_intersection_indices, inlusive_intersection_ids)
    # assert max(all_inlusive_intersection_ids) < nrow(data)
    # assert !any(duplicated(all_inlusive_intersection_ids))
    # assert length(all_inlusive_intersection_ids) == sum(colSums(inclusive_intersection))
    # assert sum(data$in_inclusive_intersection) == sum(colSums(inclusive_intersection))

    data[, 'in_exclusive_union'] = data$in_exclusive_intersection
    data[exclusive_union_ids, 'in_exclusive_union'] = 1
    note_time('calculated modes')

    plot_intersections_subset = names(intersections_by_size)
    plot_sets_subset = intersect

    if (min_size > 0 || max_size != Inf || min_degree > 0 || max_degree != Inf || !is.null(n_intersections)) {

        # once the unused intersections are removed, we need to decide
        # if the groups not participating in any of the intersections should be kept or removed
        if (!keep_empty_groups) {
            # see: https://github.com/krassowski/complex-upset/issues/90
            itersect_data = data.frame(
                intersections_matrix[names(intersections_by_size_trimmed), ] == 1,
                check.names=FALSE,
                check.rows=FALSE
            )

            is_non_empty = sapply(itersect_data, any)
            empty_groups = names(itersect_data[!is_non_empty])

            if (length(empty_groups) != 0 && warn_when_dropping_groups) {
                to_display = ifelse(
                    length(empty_groups) <= 5,
                    paste('Dropping empty groups:', paste(empty_groups, collapse=', ')),
                    paste('Dropping', length(empty_groups), 'empty groups')
                )
                warning(to_display)
            }
            intersect_subset = intersect[!(intersect %in% empty_groups)]
        } else {
            intersect_subset = intersect
        }

        intersections_by_size = intersections_by_size_trimmed
        for (mode in names(sizes)) {
            sizes[[mode]] = sizes[[mode]][names(sizes[[mode]]) %in% names(intersections_by_size_trimmed)]
        }

        intersect = intersect_subset

        plot_intersections_subset = names(intersections_by_size_trimmed)
        plot_sets_subset = intersect_subset
    }
    if (specific_intersections) {
        plot_intersections_subset = plot_intersections_subset[plot_intersections_subset %in% intersections]
    }
    note_time('trimmed')

    stacked = stack(data[original_data_indices, ], intersect)
    stacked$id = rep(original_data_indices, length(intersect))
    stacked = stacked[stacked$values == TRUE, ]

    # Note: we do want to include the additional attributes as those provide info for filling set sizes
    metadata = data[
        match(
            stacked$id,
            original_data_indices
        ),
        setdiff(colnames(data), intersect),
        drop=FALSE
    ]

    stacked = cbind(stacked, metadata)

    names(stacked)[names(stacked) == 'ind'] = 'group'
    groups_by_size = table(stacked$group)
    groups_by_size[NOT_IN_KNOWN_SETS] = sum(data[original_data_indices, 'intersection'] == NOT_IN_KNOWN_SETS)

    note_time('stacked')

    if (sort_sets != FALSE) {
        groups_by_size = groups_by_size[get_sort_order(list(groups_by_size), sort_sets)]
    } else {
        groups_by_size = groups_by_size[names(groups_by_size)]
    }

    sorted_groups_with_not_in_known_sets = names(groups_by_size)
    sorted_groups = sorted_groups_with_not_in_known_sets[
        sorted_groups_with_not_in_known_sets != NOT_IN_KNOWN_SETS
    ]

    sort_order = NULL

    if (sort_intersections != FALSE) {

        sort_values = lapply(
            sort_intersections_by,
            function(by) {
                if (by == 'cardinality') {
                    sort_value = intersections_by_size
                } else if (by == 'degree') {
                    original_intersections_names = names(intersections_by_size)
                    sort_value = calculate_degree(original_intersections_names)
                    names(sort_value) = original_intersections_names
                } else if (by == 'ratio') {
                    sort_value = (
                        sizes[[sort_ratio_numerator]][names(intersections_by_size)]
                        /
                        sizes[[sort_ratio_denominator]][names(intersections_by_size)]
                    )
                }
                sort_value
            }
        )

        sort_order = get_sort_order(sort_values, sort_intersections)
    } else if (specific_intersections) {
        sort_order = rev(match(intersections, names(intersections_by_size)))
    }

    if (!is.null(sort_order)) {
        intersections_by_size = intersections_by_size[sort_order]

         for (mode in names(sizes)) {
            sizes[[mode]] = sizes[[mode]][names(intersections_by_size)]
        }
    }

    unique_sorted_intersections = names(intersections_by_size)
    rm(intersections_by_size)
    note_time('sorted')

    unique_intersection_members = get_intersection_members(unique_sorted_intersections)
    names(unique_intersection_members) = unique_sorted_intersections

    if (group_by == 'degree') {
        sorted_intersections = unique_sorted_intersections
    } else if (group_by == 'sets') {
        # failed refactoring attempt 1 note:
        # returning a (named) list with lapply and rbind has comparable (marginally worse)
        # time performance and worse memory performance
        # failed refactoring attempt 2 note:
        # using outer does not work here as difficult to vectorize just yet

        intersections_indices = list()
        new_intersections_ids = list()
        old_intersections_ids = list()
        lead_groups = list()
        i = 0
        new_indices = 1:nrow(data)
        indices_by_intersection = split(new_indices, data$intersection)

        for (group in sorted_groups_with_not_in_known_sets) {

            for (intersection in names(unique_intersection_members)) {
                i_groups = unique_intersection_members[[intersection]]

                if (group %in% i_groups) {
                    i = i + 1

                    old_intersections_ids[[i]] = intersection
                    lead_groups[[i]] = group
                    intersections_indices[[i]] = indices_by_intersection[[intersection]]
                    new_intersections_ids[[i]] = paste(c(group, i_groups[i_groups != group]), collapse='-')
                }
            }
        }

        lengths = sapply(intersections_indices, length)

        new_intersections_ids = unlist(new_intersections_ids)
        old_intersections_ids = unlist(old_intersections_ids)

        plot_intersections_subset = new_intersections_ids[old_intersections_ids %in% plot_intersections_subset]
        sorted_intersections = new_intersections_ids

        for (mode in names(sizes)) {
            sizes[[mode]][new_intersections_ids] = sizes[[mode]][old_intersections_ids]
        }

        data = data[unlist(intersections_indices), ]
        data$intersection = unlist(rep(new_intersections_ids, times=lengths))
        data$group_by_group = unlist(rep(lead_groups, times=lengths))

        unique_intersection_members = unique_intersection_members[old_intersections_ids]
        names(unique_intersection_members) = new_intersections_ids
    }
    note_time('grouped')

    intersections_as_groups = unique_intersection_members

    matrix_data = compute_matrix(intersections_as_groups, sorted_groups)

    group = rownames(matrix_data)

    matrix_frame = gather(
        cbind(group, matrix_data),
        'group',
        'intersection',
        'value'
    )

    if (group_by == 'sets') {
        # the set (group) by which the intersections were grouped is stored as the first element of "intersection"
        # extract first element of intersection:

        intersection_to_group = lead_groups
        names(intersection_to_group) = new_intersections_ids
        matrix_frame$group_by_group = unlist(intersection_to_group[as.character(matrix_frame$intersection)])
    }

    # restore the previous column names
    colnames(data)[colnames(data) %in% intersect] <- non_sanitized_labels[intersect]

    for (mode in names(sizes)) {
        column_name = paste0(mode, size_columns_suffix)
        data[[column_name]] = as.numeric(
            sizes[[mode]][data$intersection]
        )
    }

  note_time('finished')

  list(
    with_sizes=data,
    sets_ordering_in_ids=intersect,
    presence=stacked,
    matrix=matrix_data,
    matrix_frame=matrix_frame,
    sorted=list(
      groups=sorted_groups,
      intersections=sorted_intersections
    ),
    sizes=sizes,
    plot_intersections_subset=plot_intersections_subset,
    plot_sets_subset=plot_sets_subset,
    sanitized_labels=sanitized_labels,
    non_sanitized_labels=non_sanitized_labels
  )
}

#' Create an example dataset with three sets: A, B and C
#'
#' @export
create_upset_abc_example = function() {
    data.frame(
        # 1) 50 in A only, 2) 50 in B only, 3) 200 in C only
        # 4) 10 in A-B only, 5) 6 in A-C only, 6) 6 in B-C only
        # 7) 1 in A-B-C only, 8) 2 in neither
        A = c(
            # 1) 50 in A only
            rep(T, 50),
            # 2) 50 in B only
            rep(F, 50),
            # 3) 200 in C only
            rep(F, 200),
            # 4) 10 in A-B only
            rep(T, 10),
            # 5) 6 in A-C only
            rep(T, 6),
            # 6) 6 in B-C only
            rep(F, 6),
            # 7) 1 in A-B-C only
            rep(T, 1),
            # 8) 2 in neither
            rep(F, 2)
        ),
        B = c(
            # 1) 50 in A only
            rep(F, 50),
            # 2) 50 in B only
            rep(T, 50),
            # 3) 200 in C only
            rep(F, 200),
            # 4) 10 in A-B only
            rep(T, 10),
            # 5) 6 in A-C only
            rep(F, 6),
            # 6) 6 in B-C only
            rep(T, 6),
            # 7) 1 in A-B-C only
            rep(T, 1),
            # 8) 2 in neither
            rep(F, 2)
        ),
        C = c(
            # 1) 50 in A only
            rep(F, 50),
            # 2) 50 in B only
            rep(F, 50),
            # 3) 200 in C only
            rep(T, 200),
            # 4) 10 in A-B only
            rep(F, 10),
            # 5) 6 in A-C only
            rep(T, 6),
            # 6) 6 in B-C only
            rep(T, 6),
            # 7) 1 in A-B-C only
            rep(T, 1),
            # 8) 2 in neither
            rep(F, 2)
        )
    )
}