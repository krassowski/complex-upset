names_of_true = function(row) {
  paste(names(which(row)), collapse="-")
}

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



upset_data = function(data, intersect, min_size=0) {
    intersect = unlist(intersect)
    if (length(intersect) == 1) {
        stop('Needs at least two indicator variables')
    }

    data$intersection = apply(
      data[intersect], 1,
      names_of_true
    )
    data$intersection[data$intersection == ''] = 'NOT_IN_EITHER_GROUP'

    intersections_by_size = table(data$intersection)
    if(min_size > 0) {
        intersections_by_size = intersections_by_size[intersections_by_size >= min_size]
        data = data[data$intersection %in% names(intersections_by_size),]
    }

    stacked = stack(data, intersect)
    stacked = stacked[stacked$values == TRUE, ]
    stacked$group = stacked$ind

    groups_by_size = table(stacked$group)
    groups_by_size = groups_by_size[order(groups_by_size)]
    sorted_groups = names(groups_by_size)

    intersections_by_size = intersections_by_size[order(intersections_by_size)]
    sorted_intersections = names(intersections_by_size)

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

    group = rownames(matrix_data)
    matrix_frame = tidyr::gather(
        cbind(group, matrix_data),
        intersection,
        value,
        -group
    )

  list(
    intersected=data,
    presence=stacked,
    matrix=matrix_data,
    matrix_frame=matrix_frame,
    sorted=list(
      groups=sorted_groups,
      intersections=sorted_intersections
    )
  )
}

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

#' @export
upset_annotate = function(y, geom) {
  list(
    aes=aes_string(x='intersection', y=y),
    geom=geom
  )
}

segment_end = function(data, intersection, end) {
  corresponding = data$matrix_frame[data$matrix_frame$intersection == intersection, ]
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

upset_stripes = c('grey95', 'white')

matrix_background_stripes = function(data, stripes, orient='horizontal') {
  if (!(orient %in% c('horizontal', 'vertical'))) {
    stop('Incorrect orient')
  }
  if (orient == 'horizontal') {
    aes = aes(x=-Inf, xend=Inf, y=group, yend=group)
  } else {
    aes = aes(y=-Inf, yend=Inf, x=group, xend=group)
  }
  list(
    geom_segment(
      data=data$matrix_frame,
      aes,
      color=ifelse(
        which(data$sorted$groups==data$matrix_frame$group) %% 2 == 0,
        stripes[[1]],
        stripes[[2]]
      ),
      size=7
    )
  )
}


intersection_size_text = list(vjust=-0.25)

#' @export
intersection_size = function(
  counts=TRUE, bar_number_threshold=0.85,
  text_colors=c(on_background='black', on_bar='white'),
  text=list(),
  aest=aes_string()
) {
  if (counts) {
    text = modifyList(intersection_size_text, text)
    text_aes = aes(
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
    )
      text
      
    counts_geoms = list(
      do.call(
        geom_text,
        c(
            list(
                stat='count',
                text_aes
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

  list(
    aes=modifyList(aes(x=intersection), aest),
    geom=list(
      geom_bar(),
      counts_geoms
    )
  )
}


upset_test = function(
    data,
    intersect,
    comparisons=list()
) {

    comparison = do.call(compare_between_intersections, c(list(data, intersect), comparisons))

    if (nrow(comparison) == 0) {
        stop('No varaibles to compare')
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

#' @export
#' #' Compose and UpSet plot
#'
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
  overall_sizes_bar_width=0.6,
  ...
) {
  annotations = c(annotations, base_annotations)

  data = upset_data(data, intersect, ...)

  overall_sizes = (
    ggplot(data$presence, aes(x=group))
    + matrix_background_stripes(data, stripes, 'vertical')
    + geom_bar(width=overall_sizes_bar_width)
    + coord_flip()
    + scale_y_reverse()
    + scale_x_discrete(limits=data$sorted$groups)
    + ylab('Set size')
    + themes$overall_sizes
  )

  intersections_matrix = (
    ggplot(data$matrix_frame, aes(x=intersection, y=group))
    + matrix_background_stripes(data, stripes)
    # the dots outline
    + geom_point(color=ifelse(data$matrix_frame$value, 'black', 'grey70'), size=3.5)
    # the dot
    + geom_point(aes(color=value), size=3)
    # interconnectors on the dots
    + geom_segment(
      aes(
        x=intersection,
        xend=intersection,
        y=segment_end(data, intersection, head),
        yend=segment_end(data, intersection, tail)
      )
    )
    + xlab(name)
    + scale_y_discrete(limits=data$sorted$groups, labels=labeller)
    + scale_x_discrete(limits=rev(data$sorted$intersections))
    + themes$intersections_matrix
  )

  rows = list()

  for (name in names(annotations)) {
    annotation = annotations[[name]]

    if (name %in% names(themes)) {
      theme = themes[[name]]
    } else {
      theme = themes[['default']]
    }

    rows[[length(rows) + 1]] = plot_spacer()
    rows[[length(rows) + 1]] = (
      ggplot(data$intersected, annotation$aes)
      + annotation$geom
      + scale_x_discrete(limits=rev(data$sorted$intersections))
      + xlab(name)
      + ylab(name)
      + theme
    )
  }

  if (length(rows)) {
    annotations_plots = Reduce(f='+', rows)
    plot = annotations_plots + overall_sizes + intersections_matrix
  } else {
    annotations_plots = list()
    plot = overall_sizes + intersections_matrix
  }

  plot = plot + plot_layout(
    widths=c(width_ratio, 1 - width_ratio),
    ncol=2,
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
