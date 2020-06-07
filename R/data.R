#' @importFrom utils stack
NULL


sanitize_names = function(variables_names) {
    sanitized_names = c()
    for (name in variables_names) {
        if (grepl('-', name, fixed=TRUE)) {
            original_name = name
            name = gsub('-', '_', name)
            if (name %in% variables_names) {
            stop(paste(
                'The group names contain minus characters (-) which prevent intersections names composition;',
                'offending group:', original_name, 'please substitute the these characters using gsub and try again.'
            ))
            }
        }
        sanitized_names = c(sanitized_names, name)
        }
    sanitized_names
}


names_of_members = function(row) {
  paste(names(which(row)), collapse='-')
}


gather = function(data, idvar, col_name, value_name='value') {
    not_idvar = colnames(data)
    not_idvar = not_idvar[not_idvar != idvar]
    result <- stack(data, select=not_idvar)
    result$group <- as.factor(rep(data[[idvar]], times=ncol(data) - 1))
    colnames(result) = c(value_name, col_name, idvar)
    result
}


compute_matrix = function(sorted_intersections, sorted_groups) {
    rows = c()
    for (group in sorted_groups) {
        row = c()
        for (intersection in sorted_intersections) {
            i_groups = unlist(strsplit(intersection, '-'))
            row = cbind(row, group %in% i_groups)
        }
        rows = rbind(rows, row)
    }

    matrix_data = as.data.frame(rows, row.names=sorted_groups)
    colnames(matrix_data) = sorted_intersections
    matrix_data
}


compute_unions = function(data, sorted_intersections) {
    rows = c()

    for (intersection in sorted_intersections) {
        i_groups = unlist(strsplit(intersection, '-'))
        union_for_intersection = data[data$group %in% i_groups, ]

        ids = union_for_intersection$id
        deduplicated = union_for_intersection[!duplicated(ids), ]

        # NOTE:
        # union: nrow(deduplicated)
        # sum of counts: nrow(union_for_intersection)
        rows = c(rows, nrow(deduplicated))
    }

    names(rows) = sorted_intersections
    rows
}


check_sort = function(
    sort_order,
    allowed = c('descending', 'ascending'),
    what = 'order'
) {

    if (sort_order == FALSE) {
        return(TRUE)
    }

    if (!(sort_order %in% allowed)) {
        stop(
            paste0(
                'Sort ',
                what,
                ' has to be one of: ',
                paste(allowed, collapse=' or '),
                ', not "',
                sort_order,
                '""'

            )
        )
    }

    TRUE
}


get_sort_order = function(data, sort_order) {
    check_sort(sort_order)

    if (sort_order == 'descending') {
        order(data)
    } else {
        order(-data)
    }
}


count_occurrences = function(x, symbol) {
    lengths(regmatches(x, gregexpr(symbol, x)))
}


trim_intersections = function(intersections_by_size, min_size=0, max_size=Inf, min_degree=0, max_degree=Inf) {
    intersections_by_size = intersections_by_size[
        (intersections_by_size >= min_size)
        &
        (intersections_by_size <= max_size)
    ]
    if (min_degree > 0 || max_degree != Inf) {
        degrees = count_occurrences(names(intersections_by_size), '-')
        intersections_by_size = intersections_by_size[
            (degrees >= min_degree)
            &
            (degrees <= max_degree)
        ]
    }
    intersections_by_size
}

#' Prepare data for UpSet plots
#'
#' @param data a dataframe including binary columns representing membership in classes
#' @param intersect which columns should be used to compose the intersection
#' @param min_size minimal number of observations in an intersection for it to be included
#' @param max_size maximal number of observations in an intersection for it to be included
#' @param min_degree minimal degree of an intersection for it to be included
#' @param max_degree minimal degree of an intersection for it to be included
#' @param keep_empty_groups whether empty sets should be kept (including sets which are only empty after filtering by size)
#' @param warn_when_dropping_groups whether a warning should be issued when empty sets are being removed
#' @param sort_sets whether to sort the rows in the intersection matrix (descending sort by default); one of: `'ascending'`, `'descending'`, `FALSE`
#' @param sort_intersections whether to sort the columns in the intersection matrix (descending sort by default); one of: `'ascending'`, `'descending'`, `FALSE`
#' @param sort_intersections_by the mode of sorting, the size of the intersection (cardinality) by default; one of: `'cardinality'`, `'degree'`, `'ratio'`
#' @param min_max_early whether the min and max limits should be applied early (for faster plotting), or late (for accurate depiction of ratios)
#' @param union_count_column name of the column to store the union size (adjust if conflicts with your data)
#' @param intersection_count_column name of the column to store the intersection size (adjust if conflicts with your data)
#' @export
upset_data = function(
    data, intersect, min_size=0, max_size=Inf, min_degree=0, max_degree=Inf,
    keep_empty_groups=FALSE, warn_when_dropping_groups=TRUE,
    sort_sets='descending',
    sort_intersections='descending',
    sort_intersections_by='cardinality',
    min_max_early=TRUE,
    union_count_column='union_size', intersection_count_column='intersection_size'
) {
    if ('tbl' %in% class(data)) {
        data = as.data.frame(data)
    }

    check_sort(sort_sets)
    check_sort(sort_intersections)

    check_sort(sort_intersections_by, allowed=c('cardinality', 'degree', 'ratio'), what='method')

    intersect = unlist(intersect)
    if (length(intersect) == 1) {
        stop('Needs at least two indicator variables')
    }

    # convert to logical if needed
    is_column_logical = sapply(data[, intersect], is.logical)
    if (any(!is_column_logical)) {
        non_logical = names(is_column_logical[is_column_logical == FALSE])
        print(paste('Converting non-logical columns to binary:', paste(non_logical, collapse=', ')))
        data[, non_logical] = sapply(data[, non_logical], as.logical)
    }

    intersect_in_order_of_data = colnames(data)[colnames(data) %in% intersect]
    colnames(data)[colnames(data) %in% intersect] <- sanitize_names(intersect_in_order_of_data)
    non_sanitized_labels = intersect
    intersect = sanitize_names(intersect)
    names(non_sanitized_labels) = intersect

    data$intersection = apply(
      data[intersect], 1,
      names_of_members
    )
    data$intersection[data$intersection == ''] = 'NOT_IN_EITHER_GROUP'

    intersections_by_size = table(data$intersection)

    plot_intersections_subset = names(intersections_by_size)
    plot_sets_subset = intersect

    if (min_size > 0 || max_size != Inf || min_degree > 0 || max_degree != Inf) {

        intersections_by_size_trimmed = trim_intersections(
            intersections_by_size,
            min_size=min_size,
            max_size=max_size,
            min_degree=min_degree,
            max_degree=max_degree
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
                print(to_display)
            }
            intersect_subset = intersect[!(intersect %in% empty_groups)]
        } else {
            intersect_subset = intersect
        }

        if (min_max_early == TRUE) {
            intersections_by_size = intersections_by_size_trimmed

            if (!keep_empty_groups) {
                intersect = intersect_subset
                data = data_subset
            }
        }

        plot_intersections_subset = names(intersections_by_size_trimmed)
        plot_sets_subset = intersect_subset
    }

    stacked = stack(data, intersect)

    stacked$id = rep(1:nrow(data), length(intersect))

    stacked = stacked[stacked$values == TRUE, ]
    stacked$group = stacked$ind

    groups_by_size = table(stacked$group)
    if (sort_sets != FALSE) {
        groups_by_size = groups_by_size[get_sort_order(groups_by_size, sort_sets)]
    } else {
        groups_by_size = groups_by_size[names(groups_by_size)]
    }
    sorted_groups = names(groups_by_size)

    if (sort_intersections != FALSE) {

        if (sort_intersections_by == 'cardinality') {
            sort_value = intersections_by_size
        } else if (sort_intersections_by == 'degree') {
            original_intersections_names = names(intersections_by_size)
            sort_value = count_occurrences(original_intersections_names, '-')
            names(sort_value) = original_intersections_names
        } else if (sort_intersections_by == 'ratio') {
           unsorted_union_sizes = compute_unions(stacked, names(intersections_by_size))
            sort_value = intersections_by_size
            sort_value = sort_value / unsorted_union_sizes
        }

        intersections_by_size = intersections_by_size[
            get_sort_order(sort_value, sort_intersections)
        ]
    }
    sorted_intersections = names(intersections_by_size)

    matrix_data = compute_matrix(sorted_intersections, sorted_groups)

    group = rownames(matrix_data)

    matrix_frame = gather(
        cbind(group, matrix_data),
        'group',
        'intersection',
        'value'
    )

   union_sizes = compute_unions(stacked, sorted_intersections)

   with_sizes = data.frame(data)

   with_sizes[[union_count_column]] = sapply(data$intersection, function(intersection) { union_sizes[intersection] })
   with_sizes[[intersection_count_column]] = sapply(data$intersection, function(intersection) { intersections_by_size[intersection] })

  list(
    with_sizes=with_sizes,
    sets_ordering_in_ids=intersect,
    intersected=data,
    presence=stacked,
    matrix=matrix_data,
    matrix_frame=matrix_frame,
    sorted=list(
      groups=sorted_groups,
      intersections=sorted_intersections
    ),
    union_sizes=union_sizes,
    plot_intersections_subset=plot_intersections_subset,
    plot_sets_subset=plot_sets_subset,
    non_sanitized_labels=non_sanitized_labels
  )
}
