#' @importFrom utils head modifyList tail
#' @importFrom ggplot2 ggplot aes aes_string coord_flip theme xlab ylab
#' @importFrom ggplot2 scale_color_manual scale_x_discrete scale_y_discrete scale_y_reverse
#' @importFrom ggplot2 geom_text geom_bar geom_col geom_point geom_segment
#' @importFrom ggplot2 is.ggplot %+%
#' @importFrom scales log_breaks trans_new
#' @importFrom patchwork plot_layout plot_spacer wrap_elements
NULL

globalVariables(c(
    'intersection',
    'group',
    'union_size',
    '..count..',
    'layer',
    'value'
))

#' List of default themes for upset components
#'
#' @export
#' @importFrom ggplot2 theme_minimal element_blank
upset_themes = list(
  intersections_matrix=list(
    theme_minimal(),
    theme(
      # hide intersections
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      # hide group title
      axis.title.y=element_blank()
    ),
    scale_color_manual(
      values=list('TRUE'='black', 'FALSE'='grey85'),
      guide=FALSE
    )
  ),
  'Intersection size'=list(
    theme_minimal(),
    theme(
      axis.text.x=element_blank(),
      axis.title.x=element_blank()
    )
  ),
  overall_sizes=list(
    theme_minimal(),
    theme(
      # hide groups
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  ),
  default=list(
    theme_minimal(),
    theme(
      axis.text.x=element_blank(),
      axis.title.x=element_blank()
    )
  )
)


#' Default themes modified by specified arguments
#'
#' Return the default UpSet themes with all themes modified with provided arguments
#'
#' @param ... arguments passed to `theme()`
#' @export
upset_default_themes = function(...)  {
    sapply(
        upset_themes, function(default) {
            c(default, list(theme(...)))
        }
    )
}


#' Default themes modified by specified component-specific arguments
#'
#' Return the default UpSet themes with specific themes modified with provided themes
#'
#' @param to_update a named list of themes to be used to modify themes of specific components; see `names(upset_themes)` for components names.
#' @export
upset_modify_themes = function(to_update)  {
    c(
        sapply(
            names(to_update),
            function(name) {
                c(upset_themes[[name]], to_update[name])
            },
            simplify=FALSE
        ),
        upset_themes[setdiff(names(upset_themes), names(to_update))]
    )
}



convert_annotation = function(...) {
    arguments = list(...)
    if (is.null(arguments$aes)) {
        intersection_plot = ggplot()
    } else {
        intersection_plot = ggplot(mapping=arguments$aes)
    }
    intersection_plot$highlight_geom = arguments$highlight_geom
    intersection_plot$top_geom = arguments$top_geom
    intersection_plot$geom = arguments$geom
    intersection_plot
}


#' Annotation panel shorthand
#'
#' Simplifies creation of annotation panels, automatically building aesthetics mappings,
#' at a cost of lower flexibility than when providing a custom mapping; `aes(x=intersection)` is prespecified.
#'
#' @param y A string with the name of the y aesthetic
#' @param geom A geom to be used as an annotation
#' @export
upset_annotate = function(y, geom) {
  annotation = convert_annotation(
    aes=aes_string(x='intersection', y=y),
    geom=geom
  )
  annotation$default_y = y
  annotation
}

segment_end = function(matrix_frame, data, intersection, end) {
  corresponding = matrix_frame[matrix_frame$intersection == intersection, ]

  ordering_reference = data$sorted$groups
  reference_order = order(data$sorted$groups)

  h = apply(corresponding, FUN=function(row) {
    group_members = corresponding[
      corresponding$intersection == row['intersection']
      &
        corresponding$value == TRUE
      ,
      'group'
      ]
    g = ordering_reference[sort(reference_order[group_members])]

    ifelse(
      length(g),
      end(g, 1),
      NA
    )
  }, MARGIN=1)
}

upset_stripes = c('white', 'grey95')

matrix_background_stripes = function(data, stripes, orient='horizontal') {

  if (!(orient %in% c('horizontal', 'vertical'))) {
    stop('Incorrect orient')
  }
  if (orient == 'horizontal') {
    aes = aes(x=-Inf, xend=Inf, y=group, yend=group)
  } else {
    aes = aes(y=-Inf, yend=Inf, x=group, xend=group)
  }
  groups = data$sorted$groups[data$sorted$groups %in% data$plot_sets_subset]
  list(
    geom_segment(
      data=data.frame(group=groups),
      aes,
      color=rep_len(stripes, length(groups)),
      size=7
    )
  )
}


intersection_size_text = list(vjust=-0.25)


#' Barplot annotation of intersections sizes
#'
#' @param counts whether to display count number labels above the bars
#' @param bar_number_threshold if less than one, labels for bars height greater than this threshold will be placed on (not above) the bars
#' @param text_colors a name vector of characters specifying the color when `on_background` and `on_bar` (see `bar_number_threshold`)
#' @param text additional parameters passed to `geom_text`
#' @param text_aes additional aesthetics for `geom_text`
#' @param aest additional aesthetics for `geom_bar`
#' @export
intersection_size = function(
  counts=TRUE,
  bar_number_threshold=0.85,
  text_colors=c(on_background='black', on_bar='white'),
  text=list(),
  text_aes=aes_string(),
  aest=aes_string()
) {
  if (counts) {
    text = modifyList(intersection_size_text, text)
    text_aes = modifyList(
        aes(
            label=..count..,
            y=ifelse(
                ..count.. <= bar_number_threshold * max(..count..),
                ..count..,
                bar_number_threshold * ..count..
            ),
            colour=ifelse(
                ..count.. <= bar_number_threshold * max(..count..),
                'on_background',
                'on_bar'
            )
        ),
        text_aes
    )

    counts_geoms = list(
      do.call(
        geom_text,
        c(
            list(
                stat='count',
                text_aes,
                na.rm=TRUE
            ),
            text
        )
      ),
      scale_color_manual(
        values=text_colors,
        guide=FALSE
      )
    )
  } else {
    counts_geoms = list()
  }

  bar_geom = list(geom_bar(na.rm=TRUE))

  convert_annotation(
    aes=modifyList(aes(x=intersection), aest),
    geom=bar_geom,
    highlight_geom=bar_geom,
    top_geom=counts_geoms
  )
}


#' Generate percentage label of the intersection/union sizes ratio
#'
#' For use together with `intersection_size` or `intersection_ratio`
#'
#' @param digits How many digits to show when rounding the percentage?
#' @param sep set to space (`' '`) if you prefer a whitespace between the number and the `\%` sign.
#'
#' @export
#' @examples
#' ggplot2::aes_(label=upset_text_percentage())
upset_text_percentage = function(digits=0, sep='') {
    substitute(
        paste(
            round(
                intersection_size / union_size * 100,
                digits
            ),
            '%',
            sep=sep
        )
    )
}


#' Barplot annotation of relative intersections sizes
#'
#' A large intersection size can be driven by a large number of members in a group;
#' to account for that, one can divide the intersection size by the size of a union of the same groups.
#' This cannot be calculated for the null intersection (observations which do not belong to either of the groups).
#' @inheritParams intersection_size
#' @export
intersection_ratio = function(
  counts=TRUE,
  bar_number_threshold=0.75,
  text_colors=c(on_background='black', on_bar='white'),
  text=list(),
  text_aes=aes_string(),
  aest=aes_string()
) {

  if (counts) {
    text = modifyList(intersection_size_text, text)
    text_aes = modifyList(
        aes(
            label=paste(intersection_size, '/', union_size),
            y=ifelse(
                intersection_size/union_size <= bar_number_threshold * max((intersection_size/union_size)[union_size!=0]),
                intersection_size/union_size,
                bar_number_threshold * intersection_size/union_size
            ),
            colour=ifelse(
                intersection_size/union_size <= bar_number_threshold * max((intersection_size/union_size)[union_size!=0]),
                'on_background',
                'on_bar'
            )
        ),
        text_aes
    )

    counts_geoms = list(
      do.call(
        geom_text,
        c(
            list(
                text_aes,
                check_overlap=TRUE,
                na.rm=TRUE
            ),
            text
        )
      ),
      scale_color_manual(
        values=text_colors,
        guide=FALSE
      )
    )
  } else {
    counts_geoms = list()
  }

  bar_geom = list(geom_col(
      aes(y=ifelse(union_size == 0, 0, 1/union_size)),
      # does not work, see
      # https://github.com/tidyverse/ggplot2/issues/3532
      na.rm=TRUE
  ))

  convert_annotation(
    aes=modifyList(aes(x=intersection), aest),
    geom=bar_geom,
    highlight_geom=bar_geom,
    top_geom=counts_geoms
  )
}


merge_rows = function(a, b) {
    extra_b = setdiff(colnames(b), colnames(a))
    extra_a = setdiff(colnames(a), colnames(b))

    if (!is.null(extra_a) && length(extra_a) != 0) {
        b[extra_a] = NULL
    }
    if (!is.null(extra_b) && length(extra_b) != 0) {
        a[extra_b] = NULL
    }
    rbind(a, b)
}


queries_for = function(queries, component) {
    df = list()
    columns = character()

    for (query in queries) {
        if (!is.null(query$only_components) && !(component %in% query$only_components)) {
            next
        }
        query$method = ifelse(
            is.null(query$intersect),
            ifelse(
                is.null(query$set),
                'group_by_group',
                'set'
            ),
            'intersect'
        )
        query$query = query[[query$method]]
        df[[length(df) + 1]] = query
        columns = union(columns, names(query))
    }

    if (length(df) != 0) {
        for (row_id in 1:length(df)) {
            row = df[[row_id]][columns]
            names(row) = columns
            df[[row_id]] = row
        }
    }

    as.data.frame(do.call(rbind, df))
}


set_queries = function(queries) {
    queries[queries$method == 'set', ]
}


group_by_queries = function(queries) {
    queries[queries$method == 'group_by_group', ]
}


intersect_queries = function(queries, data) {
    queries = queries[queries$method == 'intersect', ]
    queries$intersect = sapply(
        queries$intersect,
        function(sets) {
            if (length(sets) == 1 && is.na(sets)) {
                'NOT_IN_EITHER_GROUP'
            } else {
                paste(data$sets_ordering_in_ids[data$sets_ordering_in_ids %in% sets], collapse='-')
            }
        }
    )
    queries$query = queries$intersect
    queries
}


without_query_columns = function(queries) {
    queries[
        ,
        !(colnames(queries) %in% c('only_components', 'set', 'intersect', 'method', 'query', 'intersection', 'value', 'group', 'group_by_group')),
        drop=FALSE
    ]
}


extract_geom_params_for = function(queries, geom, preserve_query=FALSE) {
    accepted_params = c(geom$geom$aesthetics(), geom$geom$parameters())
    accepted_params[accepted_params == 'colour'] = 'color'
    user_params = as.data.frame(without_query_columns(queries))

    params = user_params[, colnames(user_params) %in% accepted_params, drop=FALSE]

    if (nrow(unique(params)) == 1) {
        params = unique(params)
    } else {
        params = as.data.frame(
            sapply(params, function(x) { ifelse(x == 'NULL', NA, x) }),
            stringsAsFactors=FALSE,
            check.names=FALSE,
            check.optionsrows=FALSE
        )

        # filter-out all-missing columns added when merging queries
        params = Filter(function(column) { !all(is.na(column)) }, params)

        if (preserve_query) {
            params$query = queries$query
        }
    }

    params
}


get_highlights_data = function(data, key, queries) {
    if (nrow(queries) > 0) {
        merge(
            data,
            queries[, !(colnames(queries) %in% c('set', 'intersect', 'group_by_group'))],
            by.x=key,
            by.y='query',
            all.y=TRUE
        )
    } else {
        data.frame()
    }
}


highlight_layer = function(geom, data, args=list()) {
    if (nrow(data) == 0) {
        list()
    } else {

        list(
            do.call(
                geom,
                modifyList(
                    c(list(data=data), args),
                    extract_geom_params_for(data, geom())
                )
            )
        )
    }
}


#' Generate mapping for labeling percentages
#'
#' @param relative_to defines proportion that should be calculated, relative to `'intersection'`, `'group'`, or `'all'` observed values
#' @param digits number of digits to show (default=0)
#' @param sep separator separator between the digit and percent sign (no separator by default)
#' @export
aes_percentage = function(relative_to, digits=0, sep='') {
    check_argument(relative_to, allowed=c('intersection', 'group', 'all'), description='relative_to')

    switch(
        relative_to,
        intersection=substitute(
            paste(round(100 * ..count../tapply(..count.., ..x.., sum)[..x..], digits), '%', sep=sep)
        ),
        group=substitute(
            paste(round(100 * ..prop.., digits), '%', sep=sep)
        ),
        all=substitute(
            paste(round(100 * ..count../sum(..count..), digits), '%', sep=sep)
        )
    )
}


#' Highlight chosen sets or intersections
#'
#' Highlight sets or intersections matching specified query.
#'
#' @param set name of the set to highlight
#' @param intersect a vector of names for the intersection to highlight; pass 'NA' to select the empty intersection
#' @param group name of the set to highlight when using group_by='sets'
#' @param only_components which components to modify; by default all eligible components will be modified; the available components are 'overall_sizes', 'intersections_matrix', 'Intersection size', and any annotations specified
#' @param ... - passed to geoms in modified components
#' @export
#' @examples
#' upset_query(intersect=c('Drama', 'Comedy'), color='red', fill='red')
#' upset_query(set='Drama', fill='blue')
upset_query = function(set=NULL, intersect=NULL, group=NULL, only_components=NULL, ...) {
    passed_count = sum(c(!is.null(set), !is.null(intersect), !is.null(group)))
    if (passed_count > 1) {
        stop('Please pass only one of: "set", "intersect", or "group"')
    }
    if (passed_count == 0) {
        stop('Please pass "set", "intersect", or "group"')
    }
    list(set=set, intersect=intersect, group_by_group=group, only_components=only_components, ...)
}



preserve_infinite = function(f) {
    # this allows the stripes (which span from -Inf to +Inf) to be displayed
    function(x) {
        if (!any(is.na(x)) && (all(x == Inf) || all(x == -Inf))) {
            x
        } else {
            f(x)
        }
    }
}

#' Logarithmic scale for use with `upset_set_size()`
#'
#' Inspired by [Brian Diggs' answer](https://stackoverflow.com/a/11054781) which is CC-BY-SA 4.0.
#'
#' @param base logarithm base (default 10)
#' @export
reverse_log_trans = function(base=10) {
    trans_new(
        paste0('reverselog-', base),
        preserve_infinite(function(x) -log(x, base)),
        preserve_infinite(function(x) base^-x),
        log_breaks(base=base),
        domain=c(1e-100, Inf)
    )
}


#' Prepare layers for sets sizes plot
#'
#' @param geom the geom to use
#' @param layers a list of additional layers (scales, geoms) to be included on the plot
#' @param mapping additional aesthetics
#' @param ... passed to the geom
#' @export
upset_set_size = function(geom=geom_bar, layers=list(), mapping=aes(), ...) {
    args = eval(list(...))

    convert_annotation(
        geom=list(
            geom(...)
        ),
        aes=mapping,
        highlight_geom=list(
            geom(...)
        )
    ) + ylab('Set size')
}


add_highlights_to_geoms = function(geoms, highlight_geoms, highlight_data, annotation_queries, kind='intersection') {
    geoms_plus_highlights = list()

    for (geom in geoms) {
        geoms_plus_highlights[[length(geoms_plus_highlights) + 1]] = geom
    }

    for (geom in highlight_geoms) {
        if (is.null(geom$geom)) {
            # TODO and on accepted geoms list if any
            next
        }
        highlight_geom = geom

        params_in_geom_and_stat = intersect(
            names(geom$geom_params),
            names(geom$stat_params)
        )

        # if there is a param in both stat and geom, and value is the same, remove it from stat params
        # as otherwise we will get a spurious warning from ggplot: "Duplicated aesthetics after name standardisation"
        stat_params = geom$stat_params
        if (length(params_in_geom_and_stat) != 0) {
            for (shared_param in params_in_geom_and_stat) {
                if (!identical(geom$geom_params[[shared_param]], geom$stat_params[[shared_param]])) {
                    warning(paste0('A param in both geom and stat differs in value: ', shared_param))
                    next
                }
                stat_params = stat_params[names(stat_params) != shared_param]
            }
        }
        params = extract_geom_params_for(annotation_queries, geom, preserve_query=TRUE)

        if (!is.null(params$query)) {
            non_unique = duplicated(params$query)
            if (sum(non_unique) != 0) {
                stop(paste('The queries are not unique:', params$query[non_unique]))
            }
            # reorder to match the data order:
            params = params[
                match(unique(highlight_data[, kind]), params$query),
                colnames(params) != 'query',
                drop=FALSE
            ]
        }

        highlight_geom$geom_params = modifyList(
            geom$geom_params,
            lapply(params, unlist)
        )

        geoms_plus_highlights[[length(geoms_plus_highlights) + 1]] = layer(
            geom=highlight_geom$geom,
            params=c(geom$aes_params, highlight_geom$geom_params, stat_params),
            stat=highlight_geom$stat,
            data=highlight_data,
            mapping=highlight_geom$mapping,
            position=highlight_geom$position
        )
    }

    geoms_plus_highlights
}

                          
scale_if_missing = function(annotation, axis, scale) {
    user_y_scales = lapply(annotation$scales$scales, function(scale_candidate) {
        axis %in% scale_candidate$aesthetics
    })

    if (length(user_y_scales) == 0) {
        list(scale)
    }
}
                          

#' Compose an UpSet plot
#' @inheritParams upset_data
#' @param name the label shown below the intersection matrix
#' @param annotations a named list of annotations, each being a list with: `list(aes=mapping, geom=geom or list of geoms)`;
#'  * (optional) `highlight_geom=list of geoms` geoms which can be highlighted with queries,
#'  * (optional) `top_geom=list of geoms` which should show up on top of highlighted queries.
#' @param base_annotations a named list with default annotations (i.e. the intersection size barplot)
#' @param themes a named list of themes for components and annotations, see `upset_default_themes()`/`upset_modify_themes()`
#' @param queries a list of queries generated with `upset_query()`
#' @param labeller function modifying the names of the sets (rows in the matrix)
#' @param height_ratio ratio of the intersection matrix to intersection size height
#' @param width_ratio ratio of the overall set size width to intersection matrix width
#' @param stripes a characters vector, specifying the background colors for rows (e.g. odd and even if two elements)
#' @param dot_size size of the points on the intersection matrix
#' @param set_sizes a list of layers defining the overall set sizes, e.g. from `upset_set_size()` (`FALSE` to hide)
#' @param wrap whether the plot should be wrapped into a group (makes adding a tile/combining with other plots easier)
#' @inheritDotParams upset_data
#' @export
upset = function(
  data,
  intersect,
  base_annotations=list(
    'Intersection size'=intersection_size(counts=TRUE)
  ),
  name='group',
  annotations=list(),
  themes=upset_themes,
  stripes=upset_stripes,
  labeller=identity,
  height_ratio=0.5,
  width_ratio=0.3,
  wrap=FALSE,
  set_sizes=upset_set_size(width=0.6),
  queries=list(),
  dot_size=3,
  ...
) {
  annotations = c(annotations, base_annotations)

  data = upset_data(data, intersect, ...)

  intersections_sorted = rev(data$sorted$intersections)
  intersections_limits = intersections_sorted[intersections_sorted %in% data$plot_intersections_subset]

  scale_intersections = scale_x_discrete(limits=intersections_limits)

  sets_limits = data$sorted$groups[data$sorted$groups %in% data$plot_sets_subset]

  show_overall_sizes = !(inherits(set_sizes, 'logical') && set_sizes == FALSE)

  matrix_intersect_queries = intersect_queries(queries_for(queries, 'intersections_matrix'), data)
  matrix_group_by_queries = group_by_queries(queries_for(queries, 'intersections_matrix'))

  intersection_query_matrix = get_highlights_data(data$matrix_frame, 'intersection', matrix_intersect_queries)
  group_query_matrix = get_highlights_data(data$matrix_frame, 'group_by_group', matrix_group_by_queries)

  query_matrix = merge_rows(
      intersection_query_matrix,
      group_query_matrix
  )

  query_matrix = query_matrix[query_matrix$value == TRUE, ]

  matrix_frame = data$matrix_frame[data$matrix_frame$group %in% data$plot_sets_subset, ]

  intersections_matrix = (
    ggplot(matrix_frame, aes(x=intersection, y=group))
    + matrix_background_stripes(data, stripes)
    # the dots outline
    + geom_point(color=ifelse(matrix_frame$value, 'black', 'grey70'), size=dot_size * 7/6, na.rm=TRUE)
    # the dot
    + geom_point(aes(color=value), size=dot_size, na.rm=TRUE)
    # the highlighted dot
    + highlight_layer(
        geom_point,
        query_matrix,
        args=list(size=dot_size)
    )
    # interconnectors on the dots
    + geom_segment(aes(
          x=intersection,
          xend=intersection,
          y=segment_end(matrix_frame, data, intersection, head),
          yend=segment_end(matrix_frame, data, intersection, tail)
    ), na.rm=TRUE)
    # highlighted interconnectors
    + highlight_layer(
        geom_segment,
        query_matrix,
        args=list(
            aes(
                x=intersection,
                xend=intersection,
                y=segment_end(query_matrix, data, intersection, head),
                yend=segment_end(query_matrix, data, intersection, tail)
             ),
             na.rm=TRUE
        )
    )
    + xlab(name)
    + scale_y_discrete(limits=sets_limits, labels=function(sets) { labeller(data$non_sanitized_labels[sets]) })
    + scale_intersections
    + themes$intersections_matrix
  )

  rows = list()

  for (name in names(annotations)) {
    annotation = annotations[[name]]
    geoms = annotation$geom

    if (!inherits(geoms, 'list')) {
        geoms = list(geoms)
    }

    annotation_queries = intersect_queries(queries_for(queries, name), data)

    if (nrow(annotation_queries) != 0) {
        highlight_data = merge(data$with_sizes, annotation_queries, by.x='intersection', by.y='intersect', all.y=TRUE)

        if (is.null(annotation$highlight_geom)) {
            highlight_geom = geoms
        } else {
            highlight_geom = annotation$highlight_geom
            if (!inherits(highlight_geom, 'list')) {
                highlight_geom = list(highlight_geom)
            }
        }

        geoms_plus_highlights = add_highlights_to_geoms(geoms, highlight_geom, highlight_data, annotation_queries)
    } else {
        geoms_plus_highlights = geoms
    }

    if (!is.null(annotation$top_geom)) {
        geoms_plus_highlights = c(geoms_plus_highlights, annotation$top_geom)
    }

    if (name %in% names(themes)) {
      selected_theme = themes[[name]]
    } else {
      selected_theme = themes[['default']]
    }

    if (show_overall_sizes) {
        rows[[length(rows) + 1]] = plot_spacer()
    }

    if (is.ggplot(annotation)) {
        annotation_plot = annotation %+% data$with_sizes
        user_theme = annotation_plot$theme
        annotation_plot = annotation_plot + selected_theme + do.call(theme, user_theme)

        if (is.null(annotation_plot$labels$y) || (!is.null(annotation_plot$default_y) && annotation_plot$default_y == annotation_plot$labels$y)) {
            annotation_plot = annotation_plot + ylab(name)
        }
    } else {
        annotation_plot = ggplot(data$with_sizes, annotation$aes) + selected_theme + xlab(name) + ylab(name)
    }

    rows[[length(rows) + 1]] = (
      annotation_plot
      + geoms_plus_highlights
      + scale_intersections
    )
  }

  if (show_overall_sizes) {
      overall_sizes_queries = set_queries(queries_for(queries, 'overall_sizes'))
      overall_sizes_highlights_data = get_highlights_data(data$presence, 'group', overall_sizes_queries)

      if (nrow(overall_sizes_queries) != 0) {
        highlight_geom = set_sizes$highlight_geom
        if (!inherits(highlight_geom, 'list')) {
            highlight_geom = list(highlight_geom)
        }
        geom = add_highlights_to_geoms(
            set_sizes$geom,
            highlight_geom,
            overall_sizes_highlights_data,
            overall_sizes_queries,
            kind='group'
        )
      } else {
          geom = set_sizes$geom
      }

      overall_sizes = (
        set_sizes %+% data$presence[data$presence$group %in% data$plot_sets_subset, ]
        + aes(x=group)
        + themes$overall_sizes
        + do.call(theme, set_sizes$theme)
        + matrix_background_stripes(data, stripes, 'vertical')
        + coord_flip()
        + geom
        + scale_x_discrete(limits=sets_limits)
        + scale_if_missing(set_sizes, axis='y', scale=scale_y_reverse())
      )

      matrix_row = list(overall_sizes, intersections_matrix)
  } else {
      matrix_row = list(intersections_matrix)
  }

  if (length(rows)) {
    annotations_plots = Reduce(f='+', rows)
    matrix_row = c(list(annotations_plots), matrix_row)
  } else {
    annotations_plots = list()
  }

  plot = Reduce(f='+', matrix_row)

  if (show_overall_sizes) {
      width_ratios = c(width_ratio, 1 - width_ratio)
  } else {
      width_ratios = 1
  }

  plot = plot + plot_layout(
    widths=width_ratios,
    ncol=1 + ifelse(show_overall_sizes, 1, 0),
    nrow=length(annotations) + 1,
    heights=c(
      rep(1, length(annotations)),
      height_ratio
    )
  )

  if (wrap) {
    wrap_elements(plot)
  } else {
    plot
  }
}
