#' @importFrom utils head modifyList tail
#' @importFrom ggplot2 ggplot aes aes_string coord_flip theme xlab ylab guide_legend
#' @importFrom ggplot2 scale_color_manual scale_x_discrete scale_y_discrete scale_y_reverse scale_y_continuous
#' @importFrom ggplot2 geom_text geom_bar geom_col geom_point geom_segment layer position_stack stat_summary
#' @importFrom ggplot2 is.ggplot %+% sym expr ggproto Stat quo_name
#' @importFrom scales log_breaks trans_new
#' @importFrom patchwork plot_layout plot_spacer guide_area wrap_elements
NULL

globalVariables(c(
    'intersection',
    'group',
    '..count..',
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
        },
        simplify=FALSE
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
  data = data.frame(
      group=groups,
      group_name=data$non_sanitized_labels[groups]
  )

  if (!is.null(stripes$data)) {
      data = merge(
          data,
          stripes$data,
          by.x='group_name',
          by.y='set',
          all.x=TRUE
      )
  }

  params = list(
      data=data,
      mapping=modifyList(aes, stripes$mapping)
  )

  if (!is.null(stripes$colors) && is.null(names(stripes$colors))) {
      params$color = rep_len(stripes$colors, length(groups))
  }

  list(
    do.call(geom_segment, params) * stripes$geom
  )
}


#' Define appearence of the stripes
#'
#' @param mapping additional aesthetics
#' @param geom a geom to use, should accept `x`, `y`, `xend`, `yend` and `color` aesthetics
#' @param colors a vector of colors to repeat as many times as needed for the fill of stripes, or a named vector specifying colors for values of the variable mapped to the color aesthetics in the mapping argument
#' @param data the dataset describing the sets with a column named `set` and any other columns as needed for mapping
#' @export
upset_stripes = function(mapping=aes(), geom=geom_segment(size=7), colors=c('white', 'grey95'), data=NULL) {
    stripes = list(
        mapping=mapping,
        geom=geom,
        colors=colors,
        data=data
    )
    class(stripes) = 'upset_stripes'
    stripes
}


intersection_size_text = list(vjust=-0.25)

#' Retrieve symbol for given mode that can be used in aesthetics mapping with double bang (`!!`)
#'
#' @param mode the mode to use. Accepted values: `exclusive_intersection` (alias `distinct`), `inclusive_intersection` (alias `intersect`), `inclusive_union` (alias `union`), `exclusive_union`.
#' @param suffix the column suffix in use as passed to `upset_data()`
#' @export
get_size_mode = function(mode, suffix='_size') {
    mode = solve_mode(mode)
    sym(paste0(mode, suffix))
}


get_mode_presence = function(mode, prefix='in_', symbol=TRUE) {
    column = paste0(prefix, solve_mode(mode))
    if (symbol) {
        sym(column)
    } else {
        column
    }
}


StatMode = ggproto(
    "StatMode",
    Stat,
    compute_group = function(data, scales, params, mode='exclusive_intersection') {
        data
    }
)


#' Layer defining the intersection mode for the data to be displayed
#'
#' By default the annotations are given data corresponding to the same mode as the mode of the passed in the `upset()` call.
#'
#' @param mode region selection mode, defines which mode data will be made available for the annotation. See `get_size_mode()` for accepted values.
#' @export
upset_mode = function(mode) {
  layer(
    stat = StatMode, data = NULL, mapping = NULL, geom = "blank",
    position = "identity", show.legend = FALSE, inherit.aes = TRUE,
    params = list(mode=solve_mode(mode))
  )
}


#' Barplot annotation of intersections sizes
#'
#' @param counts whether to display count number labels above the bars
#' @param bar_number_threshold if less than one, labels for bars height greater than this threshold will be placed on (not above) the bars
#' @param text_colors a name vector of characters specifying the color when `on_background` and `on_bar` (see `bar_number_threshold`)
#' @param text additional parameters passed to `geom_text()`
#' @param text_mapping additional aesthetics for `geom_text()`
#' @param mapping additional aesthetics for `geom_bar()`
#' @param mode region selection mode, defines which intersection regions will be accounted for when computing the size. See `get_size_mode()` for accepted values.
#' @param position position passed to `geom_bar()`
#' @inheritDotParams ggplot2::geom_bar
#' @export
intersection_size = function(
    mapping=aes(),
    counts=TRUE,
    bar_number_threshold=0.85,
    text_colors=c(on_background='black', on_bar='white'),
    text=list(),
    text_mapping=aes(),
    mode='distinct',
    position=position_stack(),
    ...
) {
  size = get_size_mode(mode)

    lab = switch(
        mode,
        exclusive_intersection='Intersection size',
        inclusive_intersection='Inclusive intersection size',
        inclusive_union='Union size',
        exclusive_union='Exclusive union size'
    )

  if (counts) {
    text = modifyList(intersection_size_text, text)
    text_mapping = modifyList(
        aes(
            label=!!size,
            y=ifelse(
                !!size <= bar_number_threshold * max(!!size, na.rm=TRUE),
                !!size,
                bar_number_threshold * !!size
            ),
            colour=ifelse(
                !!size <= bar_number_threshold * max(!!size, na.rm=TRUE),
                'on_background',
                'on_bar'
            )
        ),
        text_mapping
    )

    counts_geoms = list(
      do.call(
        geom_text,
        c(
            list(
                stat='unique',
                text_mapping,
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

  bar_geom = list(
      stat_summary(
          fun=sum,
          geom='bar',
          position=position,
          na.rm=TRUE,
          ...
      )
  )

  convert_annotation(
    aes=modifyList(
        aes(
            x=intersection,
            y=!!get_mode_presence(mode)
        ),
        mapping
    ),
    geom=bar_geom,
    highlight_geom=bar_geom,
    top_geom=counts_geoms
  ) + ylab(lab) + upset_mode(mode)
}


#' Generate percentage label of the intersection/union sizes ratio
#'
#' For use together with `intersection_size` or `intersection_ratio`
#'
#' @param digits How many digits to show when rounding the percentage?
#' @param sep set to space (`' '`) if you prefer a whitespace between the number and the `\%` sign.
#' @param mode region selection mode for computing the numerator in ratio. See `get_size_mode()` for accepted values.
#'
#' @export
#' @examples
#' ggplot2::aes(label=!!upset_text_percentage())
upset_text_percentage = function(digits=0, sep='', mode='distinct') {
    size = get_size_mode(mode)
    expr(
        paste(
            round(
                !!size / !!get_size_mode('inclusive_union') * 100,
                !!digits
            ),
            '%',
            sep=!!sep
        )
    )
}


#' Barplot annotation of relative intersections sizes
#'
#' A large intersection size can be driven by a large number of members in a group;
#' to account for that, one can divide the intersection size by the size of a union of the same groups.
#' This cannot be calculated for the null intersection (observations which do not belong to either of the groups).
#' @param denominator_mode region selection mode for computing the denominator in ratio. See `get_size_mode()` for accepted values.
#' @inheritParams intersection_size
#' @inheritDotParams intersection_size
#' @export
intersection_ratio = function(
  mapping=aes(),
  counts=TRUE,
  bar_number_threshold=0.75,
  text_colors=c(on_background='black', on_bar='white'),
  text=list(),
  text_mapping=aes(),
  mode='distinct',
  denominator_mode='union',
  ...
) {
  size = get_size_mode(mode)
  presence = get_mode_presence(mode)
  denominator_size = get_size_mode(denominator_mode)

  if (counts) {
    ratio = expr(!!size / !!denominator_size)

    text = modifyList(intersection_size_text, text)
    text_mapping = modifyList(
        aes(
            label=paste(!!size, '/', !!denominator_size),
            y=ifelse(
                !!ratio <= bar_number_threshold * max((!!ratio)[!!denominator_size != 0], na.rm=TRUE),
                !!ratio,
                bar_number_threshold * !!ratio
            ),
            colour=ifelse(
                !!ratio <= bar_number_threshold * max((!!ratio)[!!denominator_size != 0], na.rm=TRUE),
                'on_background',
                'on_bar'
            )
        ),
        text_mapping
    )

    counts_geoms = list(
      do.call(
        geom_text,
        c(
            list(
                text_mapping,
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
      aes(y=ifelse(
          !!denominator_size == 0,
          0,
          !!presence/!!denominator_size
      )),
      # does not work, see
      # https://github.com/tidyverse/ggplot2/issues/3532
      na.rm=TRUE,
      ...
  ))

  convert_annotation(
    aes=modifyList(aes(x=intersection), mapping),
    geom=bar_geom,
    highlight_geom=bar_geom,
    top_geom=counts_geoms
  ) + upset_mode(mode)
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


set_queries = function(queries, sanitized_labels) {
    queries = queries[queries$method == 'set', ]
    queries$query = sanitized_labels[unlist(queries$query)]
    queries
}


group_by_queries = function(queries, sanitized_labels) {
    queries = queries[queries$method == 'group_by_group', ]
    queries$group_by_group = sanitized_labels[unlist(queries$group_by_group)]
    queries$query = sanitized_labels[unlist(queries$query)]
    queries
}


intersect_queries = function(queries, data) {
    queries = queries[queries$method == 'intersect', ]

    queries$intersect = sapply(
        queries$intersect,
        function(sets) {
            if (length(sets) == 1 && is.na(sets)) {
                NOT_IN_KNOWN_SETS
            } else {
                intersection_vector_to_id(
                    sets,
                    sanitized_labels=data$sanitized_labels,
                    sets_ordering_in_ids=data$sets_ordering_in_ids
                )
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


get_highlights_data = function(data, key, queries, sanitized_labels) {
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


highlight_layer = function(geom, geom_class, data, args=list()) {
    if (nrow(data) == 0) {
        list()
    } else {

        list(
            do.call(
                geom_class,
                c(
                    list(data=data),
                    args,
                    extract_geom_params_for(data, geom_class())
                )
            ) * geom
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
#' @param intersect a vector of names for the intersection to highlight; pass `NA` to select the empty intersection
#' @param group name of the set to highlight when using `group_by='sets'`
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

    if (length(list(...)) == 0) {
        stop('Please pass at least one option or aesthetic (e.g. `color` or `fill`) to highlight the queried elements')
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
#' @param geom a geom to use
#' @param position on which side of the plot should the set sizes be displayed ('left' or 'right')
#' @param mapping additional aesthetics
#' @param filter_intersections whether the intersections filters (e.g. `n_intersections` or `min_size`) should influence displayed set sizes
#' @export
upset_set_size = function(mapping=aes(), geom=geom_bar(width=0.6), position='left', filter_intersections=FALSE) {
    check_argument(position, allowed=c('left', 'right'), description='position')

    annotation = convert_annotation(
        geom=list(geom),
        aes=mapping,
        highlight_geom=list(geom)
    ) + ylab('Set size')
    annotation$position = position
    annotation$filter_intersections = filter_intersections
    annotation
}


extract_stat_params = function(geom) {
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
    stat_params
}


`*.gg` = function(a, b) {

    a_params = c(a$aes_params, a$geom_params, extract_stat_params(a))
    b_params = c(b$aes_params, b$geom_params, extract_stat_params(b))

    if (is.null(a_params)) {
        params = b_params
    } else if (is.null(b_params)) {
        params = a_params
    } else {
        params = modifyList(
            a_params,
            b_params
        )
    }

    if (length(a$data)) {
        data = a$data
    } else if (length(b$data)) {
        data = b$data
    } else {
        data = NULL
    }

    if (is.null(b$mapping)) {
        mapping = a$mapping
    } else if (is.null(a$mapping)) {
        mapping = b$mapping
    } else {
        mapping = modifyList(a$mapping, b$mapping)
    }

    layer(
        geom=a$geom,
        params=params,
        stat=a$stat,
        data=data,
        mapping=mapping,
        position=a$position
    )
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

        stat_params = extract_stat_params(geom)

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


get_scale = function(annotation, axis) {
    candidates = unlist(sapply(
        annotation$scales$scales,
        function(scale_candidate) {
            any(axis %in% scale_candidate$aesthetics)
        }
    ))
    annotation$scales$scales[candidates]
}


scale_if_missing = function(annotation, axis, scale) {
    scales = get_scale(annotation, axis)

    if (length(scales) == 0) {
        list(scale)
    }
}
                          
#' Prepare layers for sets sizes plot
#'
#' @param geom a geom_point call, allowing to specify parameters (e.g. `geom=geom_point(shape='square')`)
#' @param segment a geom_segment call, allowing to specify parameters (e.g. `segment=geom_segment(linetype='dotted')`)
#' @param outline_color a named list with two colors for outlines of active and inactive dots
#' @export
intersection_matrix = function(
    geom=geom_point(size=3),
    segment=geom_segment(),
    outline_color=list(active='black', inactive='grey70')
) {
    plot = ggplot(mapping=aes(x=intersection, y=group))
    plot$geom = geom
    plot$segment = segment
    plot$outline_color = outline_color
    plot
}


solve_mode = function (mode) {
  check_argument(
      mode,
      allowed = c(
          'exclusive_intersection', 'distinct',
          'inclusive_intersection', 'intersect',
          'exclusive_union', # no alias
          'inclusive_union', 'union'
      ),
      'mode'
  )

    # resolve aliases
    mode = switch(
        mode,
        distinct='exclusive_intersection',
        intersect='inclusive_intersection',
        union='inclusive_union',
        mode
    )
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
#' @param stripes specification of the stripes appearance created with `upset_stripes()`
#' @param matrix the intersection matrix plot
#' @param set_sizes the overall set sizes plot, e.g. from `upset_set_size()` (`FALSE` to hide)
#' @param guides action for legends aggregation and placement ('keep', 'collect', 'over' the set sizes)
#' @param wrap whether the plot should be wrapped into a group (makes adding a tile/combining with other plots easier)
#' @param mode region selection mode for computing the number of elements in intersection fragment. See `get_size_mode()` for accepted values.
#' @param encode_sets whether set names (column in input data) should be encoded as numbers (set to TRUE to overcome R limitations of max 10 kB for variable names for datasets with huge numbers of sets); default TRUE for upset() and FALSE for upset_data().
#' @inheritDotParams upset_data
#' @export
upset = function(
  data,
  intersect,
  base_annotations='auto',
  name='group',
  annotations=list(),
  themes=upset_themes,
  stripes=upset_stripes(),
  labeller=identity,
  height_ratio=0.5,
  width_ratio=0.3,
  wrap=FALSE,
  set_sizes=upset_set_size(),
  mode='distinct',
  queries=list(),
  guides=NULL,
  encode_sets=TRUE,
  matrix=intersection_matrix(),
  ...
) {
  if (!is.null(guides)) {
     check_argument(guides, allowed = c('keep', 'collect', 'over'), 'guides')
  }

    mode = solve_mode(mode)

    if (class(base_annotations) == 'character') {
        if (base_annotations != 'auto') {
            stop('Unsupported value for `base_annotations`: provide a named list, or `"auto"`')
        } else {
            base_annotations = list(
                'Intersection size'=intersection_size(counts=TRUE, mode=mode)
            )
        }
  }

  # for backwards compatibility pre 1.2
  if (class(stripes) != 'upset_stripes') {
      stripes = upset_stripes(colors=stripes)
  }

  annotations = c(annotations, base_annotations)

  data = upset_data(data, intersect, mode=mode, encode_sets=encode_sets, ...)

  intersections_sorted = rev(data$sorted$intersections)
  intersections_limits = intersections_sorted[intersections_sorted %in% data$plot_intersections_subset]

  scale_intersections = scale_x_discrete(limits=intersections_limits)

  sets_limits = data$sorted$groups[data$sorted$groups %in% data$plot_sets_subset]

  show_overall_sizes = !(inherits(set_sizes, 'logical') && set_sizes == FALSE)

  matrix_intersect_queries = intersect_queries(queries_for(queries, 'intersections_matrix'), data)
  matrix_group_by_queries = group_by_queries(queries_for(queries, 'intersections_matrix'), data$sanitized_labels)

  intersection_query_matrix = get_highlights_data(data$matrix_frame, 'intersection', matrix_intersect_queries)
  group_query_matrix = get_highlights_data(data$matrix_frame, 'group_by_group', matrix_group_by_queries)

  query_matrix = merge_rows(
      intersection_query_matrix,
      group_query_matrix
  )

  query_matrix = query_matrix[query_matrix$value == TRUE, ]

  matrix_frame = data$matrix_frame[data$matrix_frame$group %in% data$plot_sets_subset, ]
  intersections_matrix = matrix %+% matrix_frame

  point_geom = intersections_matrix$geom

  if (!is.null(point_geom$aes_params$size)) {
      dot_size = point_geom$aes_params$size
  } else {
      dot_size = 1
  }

  geom_layers = c(
    # the dots outline
    list(intersections_matrix$geom * geom_point(
        color=ifelse(
            matrix_frame$value,
            intersections_matrix$outline_color$active,
            intersections_matrix$outline_color$inactive
        ),
        size=dot_size * 7/6,
        na.rm=TRUE
    )),
    # the dot
    list(intersections_matrix$geom * geom_point(
        aes(color=value),
        size=dot_size,
        na.rm=TRUE
    )),
    # the highlighted dot
    highlight_layer(
        intersections_matrix$geom,
        geom_point,
        query_matrix,
        args=list(size=dot_size)
    ),
    # interconnectors on the dots
    list(intersections_matrix$segment * geom_segment(aes(
          x=intersection,
          xend=intersection,
          y=segment_end(matrix_frame, data, intersection, head),
          yend=segment_end(matrix_frame, data, intersection, tail)
    ), na.rm=TRUE)),
    # highlighted interconnectors
    highlight_layer(
        intersections_matrix$segment,
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
  )

  intersections_matrix$layers = c(
      matrix_background_stripes(data, stripes),
      geom_layers,
      intersections_matrix$layers
  )
  y_scale = scale_y_discrete(
       limits=sets_limits,
       labels=function(sets) { labeller(data$non_sanitized_labels[sets]) }
  )

  user_y_scale = get_scale(intersections_matrix, 'y')

  if (length(user_y_scale) == 0) {
      user_y_scale = scale_y_discrete()
  } else {
      user_y_scale = user_y_scale[[1]]
      user_y_scale$limits = y_scale$limits
      user_y_scale$labels = y_scale$labels
      y_scale = NULL
  }

  matrix_default_colors = list('TRUE'='black', 'FALSE'='grey85')
  matrix_guide = FALSE
  matrix_breaks = names(matrix_default_colors)
  if (!is.null(names(stripes$colors))) {
      matrix_default_colors = c(
          matrix_default_colors,
          stripes$colors
      )
      matrix_guide = guide_legend()
      matrix_breaks = names(stripes$colors)
  }

  intersections_matrix = (
    intersections_matrix
    + xlab(name)
    + scale_intersections
    + y_scale
    + scale_if_missing(
          intersections_matrix,
          'colour',
          scale_color_manual(
              values=matrix_default_colors,
              guide=matrix_guide,
              breaks=matrix_breaks
          )
    )
    + themes$intersections_matrix
  )

  rows = list()

  if (show_overall_sizes) {
    is_set_size_on_the_right = !is.null(set_sizes$position) && set_sizes$position == 'right'
  }

  annotation_number = 1

  for (name in names(annotations)) {
    annotation = annotations[[name]]

    geoms = annotation$geom

    annotation_mode = mode

    for (layer in annotation$layers) {
        if (inherits(layer$stat, 'StatMode')) {
            annotation_mode = layer$stat_params$mode
        }
    }

    annotation_data = data$with_sizes[data$with_sizes[get_mode_presence(annotation_mode, symbol=FALSE)] == 1, ]

    if (!inherits(geoms, 'list')) {
        geoms = list(geoms)
    }

    annotation_queries = intersect_queries(queries_for(queries, name), data)

    if (nrow(annotation_queries) != 0) {
        highlight_data = merge(annotation_data, annotation_queries, by.x='intersection', by.y='intersect', all.y=TRUE)

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

    if (!is.null(guides) && guides == 'over' && ceiling(length(annotations) / 2) == annotation_number) {
        spacer = guide_area()
    } else {
        spacer = plot_spacer()
    }

    if (show_overall_sizes && !is_set_size_on_the_right) {
        rows[[length(rows) + 1]] = spacer
    }

    if (is.ggplot(annotation)) {
        if (is.null(annotation$mapping$x)) {
            annotation = annotation + aes(x=intersection)
        }
        annotation_plot = annotation %+% annotation_data
        user_theme = annotation_plot$theme
        annotation_plot = annotation_plot + selected_theme + do.call(theme, user_theme)

        if (is.null(annotation_plot$default_y) && !is.null(annotation_plot$mapping$y)) {
            annotation_plot$default_y = quo_name(annotation_plot$mapping$y)
        }
        if (
            is.null(annotation_plot$labels$y)
            ||
            (
                !is.null(annotation_plot$default_y)
                &&
                annotation_plot$default_y == annotation_plot$labels$y
            )
        ) {
            annotation_plot = annotation_plot + ylab(name)
        }
    } else {
        annotation_plot = ggplot(annotation_data, annotation$aes) + selected_theme + xlab(name) + ylab(name)
    }

    user_layers = annotation_plot$layers
    annotation_plot$layers = c()
    annotation_plot = annotation_plot + geoms_plus_highlights
    annotation_plot$layers = c(annotation_plot$layers, user_layers)

    rows[[length(rows) + 1]] = (
      annotation_plot
      + scale_intersections
    )

    if (show_overall_sizes && is_set_size_on_the_right) {
        rows[[length(rows) + 1]] = spacer
    }

    annotation_number =  annotation_number + 1
  }

  if (show_overall_sizes) {
      overall_sizes_queries = set_queries(queries_for(queries, 'overall_sizes'), data$sanitized_labels)
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

      if (is_set_size_on_the_right) {
          default_scale = scale_y_continuous()
      } else {
          default_scale = scale_y_reverse()
      }

      set_sizes_data = data$presence[data$presence$group %in% data$plot_sets_subset, ]

      if (set_sizes$filter_intersections) {
          set_sizes_data = set_sizes_data[set_sizes_data$intersection %in% data$plot_intersections_subset, ]
      }

      set_sizes$layers = c(
          matrix_background_stripes(data, stripes, 'vertical'),
          geom,
          set_sizes$layers
      )

      overall_sizes = (
        set_sizes %+% set_sizes_data
        + aes(x=group)
        + themes$overall_sizes
        + do.call(theme, set_sizes$theme)
        + coord_flip()
        + scale_x_discrete(limits=sets_limits)
        + scale_if_missing(set_sizes, axis='y', scale=default_scale)
        + scale_if_missing(
            set_sizes,
            'colour',
            scale_color_manual(
              values=matrix_default_colors,
              guide=FALSE
            )
        )
      )

      if (is_set_size_on_the_right) {
          matrix_row = list(intersections_matrix, overall_sizes)
      } else {
          # on the left by default
          matrix_row = list(overall_sizes, intersections_matrix)
      }
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
      if (is_set_size_on_the_right) {
          width_ratio = 1 - width_ratio
      }

      width_ratios = c(width_ratio, 1 - width_ratio)
  } else {
      width_ratios = 1
  }

  if (!is.null(guides) && guides == 'over') {
      guides = 'collect'  # guide_area() works with collect only
  }

  plot = plot + plot_layout(
    widths=width_ratios,
    ncol=1 + ifelse(show_overall_sizes, 1, 0),
    nrow=length(annotations) + 1,
    heights=c(
      rep(1, length(annotations)),
      height_ratio
    ),
    guides=guides
  )

  if (wrap) {
    wrap_elements(plot)
  } else {
    plot
  }
}
