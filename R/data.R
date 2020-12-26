#' @importFrom utils stack
NULL

EMPTY_INTERSECTION = 'NOT_IN_EITHER_GROUP'

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
        as.character(1:length(variables_names)),
        function (name) {
            while (any(name %in% avoid)) {
                name = name + 'x'
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
      EMPTY_INTERSECTION
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
    values[x == EMPTY_INTERSECTION] = 0
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
#' @param sort_ratio_numerator the mode for denominator when sorting by ratio
#' @param group_by the mode of grouping intersections; one of: `'degree'`, `'sets'`
#' @param min_max_early whether the min and max limits should be applied early (for faster plotting), or late (for accurate depiction of ratios)
#' @param mode region selection mode for sorting and trimming by size. See `get_size_mode()` for accepted values.
#' @param size_columns_suffix suffix for the columns to store the sizes (adjust if conflicts with your data)
#' @param encode_sets whether set names (column in input data) should be encoded as numbers (set to TRUE to overcome R limitations of max 10 kB for variable names for datasets with huge numbers of sets); default TRUE for upset() and FALSE for upset_data().
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
    min_max_early=TRUE,
    mode='exclusive_intersection',
    size_columns_suffix='_size',
    encode_sets=FALSE,
    max_combinations_n=24,
    intersections='observed'
) {
    # Check arguments
    mode = solve_mode(mode)

    check_argument(
        intersections,
        allowed=c('observed', 'all'),
        description='intersections'
    )

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
    if (length(intersect) == 1) {
        stop('Needs at least two indicator variables')
    }

    # Transform data

    if ('tbl' %in% class(data)) {
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

    data$intersection = apply(data[intersect], 1, names_of_members)

    unique_members_matrix = data[!duplicated(data$intersection), intersect]
    rownames(unique_members_matrix) = apply(unique_members_matrix, 1, names_of_members)

    unique_members_matrix = apply(unique_members_matrix, 1, as.numeric)

    intersections_matrix = t(unique_members_matrix)

    if (intersections == 'observed') {
        # unsorted_intersections = rownames(unique_members_matrix)
        intersections_matrix = t(unique_members_matrix)
    } else {
        if (length(intersect) > max_combinations_n && max_degree == Inf)  {
            stop('Your memory is likely to explode, please adjust max_combinations_n if you wish to proceed anyways, or better set max_degree')
        }

        if (max_degree == Inf) {
            intersections_matrix = do.call(expand.grid, rep(list(0:1), length(intersect)))
        } else {
            if (max_degree > length(intersect)) {
                warn('provided max_degree was greater than the number of sets, reducing max_degree to the number of sets')
                max_degree = length(intersect)
            }
            intersections_matrix = do.call(rbind, lapply(min_degree:max_degree, function(degree) {
                binary_grid(n=length(intersect), m=degree)
            }))
        }

        colnames(intersections_matrix) = intersect
        rownames(intersections_matrix) = apply(intersections_matrix == TRUE, 1, names_of_members)
        intersections_matrix = as.matrix(intersections_matrix)
        unique_members_matrix = t(intersections_matrix)
    }

    exclusive_intersection = table(data$intersection)
    observed_intersections = names(exclusive_intersection)

    product_matrix = intersections_matrix %*% unique_members_matrix

    if (EMPTY_INTERSECTION %in% rownames(product_matrix) && EMPTY_INTERSECTION %in% colnames(product_matrix)) {
        product_matrix[EMPTY_INTERSECTION, EMPTY_INTERSECTION] = 1
    }

    exclusive_intersection_counts = as.numeric(exclusive_intersection[colnames(product_matrix)])

    inclusive_union = apply(product_matrix != 0, 2, as.numeric) * exclusive_intersection_counts

    observed_intersections_degrees = colSums(unique_members_matrix)
    desired_intersections_degrees = rowSums(intersections_matrix)

    exclusive_union = apply(
        (product_matrix != 0) & (product_matrix >= observed_intersections_degrees),
        2,
        as.numeric
    ) * exclusive_intersection_counts

    desired_intersections_degrees[EMPTY_INTERSECTION] = 1

    intersection_condition = t(t(product_matrix) >= desired_intersections_degrees)

    inclusive_intersection = apply(
        intersection_condition,
        2,
        as.numeric
    ) * exclusive_intersection_counts

    if (intersections != 'observed') {
        exclusive_intersection = t(t(product_matrix) == observed_intersections_degrees) & (product_matrix == observed_intersections_degrees)
        exclusive_intersection = apply(
            exclusive_intersection,
            2,
            as.numeric
        ) * exclusive_intersection_counts
        exclusive_intersection[is.na(exclusive_intersection)] = 0
        exclusive_intersection = colSums(exclusive_intersection)
    }


    inclusive_intersection[is.na(inclusive_intersection)] = 0
    exclusive_union[is.na(exclusive_union)] = 0
    inclusive_union[is.na(inclusive_union)] = 0

    sizes = list(
        exclusive_intersection=exclusive_intersection,
        inclusive_intersection=colSums(inclusive_intersection),
        exclusive_union=colSums(exclusive_union),
        inclusive_union=colSums(inclusive_union)
    )

    intersections_by_size = sizes[[mode]]

    rownames(inclusive_union) = rownames(product_matrix)
    selected_intersections = intersect(colnames(inclusive_union), observed_intersections)

    original_data_indices = 1:nrow(data)
    indices_by_exclusive_intersection = split(original_data_indices, data$intersection)

    inclusive_union_indices = lapply(colnames(inclusive_union), function(region) {
        counts = inclusive_union[selected_intersections[selected_intersections != region], region]
        non_empty_subregions = names(counts[counts != 0])

        unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))
    })

    ## assert sapply(indices, length)) == colSums(inclusive_union[, union_to_be_added])

    lengths = sapply(inclusive_union_indices, length)
    all_indices = c(original_data_indices, unlist(inclusive_union_indices))
    offsets = cumsum(c(length(original_data_indices), lengths))
    names(offsets) = c(colnames(inclusive_union), NaN)


    # the initial length(original_data_indices) entries are only for regions of exclusive intersections
    # and indices here do not need any additional addressing offset. Following indices are for regions
    # that are not exclusive and require additional offest as follows:

    rownames(inclusive_intersection) = rownames(product_matrix)

    inclusive_intersections_counts = inclusive_intersection[
        intersect(colnames(inclusive_intersection), observed_intersections),
    ]
    names(inclusive_union_indices) = colnames(inclusive_union)

    inlusive_intersection_ids = unlist(unname(sapply(colnames(inclusive_intersection), function(region) {
        counts = inclusive_intersections_counts[, region]
        non_empty_subregions = names(counts[counts != 0])

        indices_in_input_space = unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))

        additional_indices = which(inclusive_union_indices[[region]] %in% indices_in_input_space)
        offsets[[region]] + additional_indices
    })))


    rownames(exclusive_union) = rownames(product_matrix)
    exclusive_intersections_counts = exclusive_union[intersect(colnames(exclusive_union), observed_intersections), ]

    exclusive_union_ids = unlist(unname(sapply(colnames(exclusive_union), function(region) {
        counts = exclusive_intersections_counts[, region]
        non_empty_subregions = names(counts[counts != 0])

        indices_in_input_space = unlist(unname(indices_by_exclusive_intersection[non_empty_subregions]))

        additional_indices = which(inclusive_union_indices[[region]] %in% indices_in_input_space)
        offsets[[region]] + additional_indices
    })))


    data = data[all_indices, ]

    data$original_index = all_indices

    data$exclusive_intersection = data$intersection[all_indices]

    data$intersection = c(
        data$intersection[original_data_indices],
        rep(colnames(inclusive_union), times=lengths)
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

    plot_intersections_subset = names(intersections_by_size)
    plot_sets_subset = intersect

    if (min_size > 0 || max_size != Inf || min_degree > 0 || max_degree != Inf || !is.null(n_intersections)) {

        intersections_by_size_trimmed = trim_intersections(
            intersections_by_size,
            min_size=min_size,
            max_size=max_size,
            min_degree=min_degree,
            max_degree=max_degree,
            n_intersections=n_intersections
        )
        data_subset = data[data$intersection %in% names(intersections_by_size_trimmed), ]

        # once the unused intersections are removed, we need to decide
        # if the groups not participating in any of the intersections should be kept or removed
        if (!keep_empty_groups) {
            itersect_data = data_subset[, intersect]
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
            } else if (max_degree < 1) {
                tip = paste0(': provide `max_degree` >= 1 (you provoided: ', max_degree, ')')
            } else {
                tip = ''
            }

            stop(paste0('No intersections left after filtering', tip))
        }

        if (min_max_early == TRUE) {
            intersections_by_size = intersections_by_size_trimmed
            for (mode in names(sizes)) {
                sizes[[mode]] = sizes[[mode]][names(sizes[[mode]]) %in% names(intersections_by_size_trimmed)]
            }

            if (!keep_empty_groups) {
                intersect = intersect_subset
                data = data_subset
            }
        }

        plot_intersections_subset = names(intersections_by_size_trimmed)
        plot_sets_subset = intersect_subset
    }

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

    stacked$group = stacked$ind
    groups_by_size = table(stacked$group)

    if (sort_sets != FALSE) {
        groups_by_size = groups_by_size[get_sort_order(list(groups_by_size), sort_sets)]
    } else {
        groups_by_size = groups_by_size[names(groups_by_size)]
    }
    sorted_groups = names(groups_by_size)

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

        intersections_by_size = intersections_by_size[sort_order]

         for (mode in names(sizes)) {
            sizes[[mode]] = sizes[[mode]][names(intersections_by_size)]
        }
    }

    unique_sorted_intersections = names(intersections_by_size)
    rm(intersections_by_size)

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

        for (group in sorted_groups) {
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
    sanitized_labels = intersect
    colnames(data)[colnames(data) %in% sanitized_labels] <- non_sanitized_labels[sanitized_labels]

    for (mode in names(sizes)) {
        column_name = paste0(mode, size_columns_suffix)
        data[[column_name]] = as.numeric(
            sizes[[mode]][data$intersection]
        )
    }

  sanitized_labels = names(non_sanitized_labels)
  names(sanitized_labels) = non_sanitized_labels

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
        # 1) 100 in A only, 2) 100 in B only, 3) 1000 in C only
        # 4) 10 in A-B only, 5) 6 in A-C only, 6) 6 in B-C only
        # 7) 1 in A-B-C only, 8) 2 in neither
        A = c(
            # 1) 100 in A only
            rep(T, 100),
            # 2) 100 in B only
            rep(F, 100),
            # 3) 1000 in C only
            rep(F, 1000),
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
            # 1) 100 in A only
            rep(F, 100),
            # 2) 100 in B only
            rep(T, 100),
            # 3) 1000 in C only
            rep(F, 1000),
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
            # 1) 100 in A only
            rep(F, 100),
            # 2) 100 in B only
            rep(F, 100),
            # 3) 1000 in C only
            rep(T, 1000),
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