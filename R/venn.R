#' @importFrom utils modifyList
#' @importFrom grDevices col2rgb rgb
#' @importFrom colorspace RGB mixcolor
#' @importFrom ggplot2 aes aes_
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#' @importFrom ggplot2 geom_label geom_polygon alpha
NULL

globalVariables(c(
    'x',
    'y',
    'region'
))


get_sets = function(data, sets=NULL) {
    if (is.null(sets)) {
        sets = colnames(data)
    } else {
        missing_sets = setdiff(sets, colnames(data))
        if (length(missing_sets) != 0) {
            stop(paste(
                'Unknown set(s):', paste(missing_sets, collapse=',')
            ))
        }
    }
    sets
}


prepare_colors = function(
    data, sets=NULL, colors=c('red', 'yellow', 'blue'),
    empty_color='grey'
) {
    sets = get_sets(data, sets)

    if (is.null(names(colors))) {
        names(colors) = sets
    }

    names(empty_color) = NOT_IN_KNOWN_SETS

    present_sets = data[, sets]
    present_sets = present_sets[!duplicated(present_sets), ]

    present_sets$color = apply(present_sets, 1, function(members) {
        present = names(members)[members]
        region_colors = colors[present]
        if (length(present) <= 1) {
            return (unname(region_colors))
        }
        alpha = 1 / length(present)

        region_colors = sapply(region_colors, function(color) {
            do.call(RGB, as.list(t(col2rgb(color))))
        })
        mixed_color = Reduce(
            f=function(color_a, color_b) {
                mixcolor(
                    alpha,
                    color_a,
                    color_b
                )
            },
            x=unname(region_colors)[2:length(present)],
            init=unname(region_colors)[[1]]
        )
        mixed_color = attr(mixed_color, 'coords')

        rgb(
            red=mixed_color[, 'R'],
            green=mixed_color[, 'G'],
            blue=mixed_color[, 'B'],
            maxColorValue=255
        )

    })
    present_sets$name = apply(present_sets[, sets], 1, names_of_members)
    mixed_colors = present_sets$color
    names(mixed_colors) = present_sets$name

    c(
        unlist(mixed_colors),
        empty_color
    )
}

#' Color scale for Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param sets vector with names of columns representing membership in sets
#' @param colors named list of colors for sets (one set=one color)
#' @param na.value value for elements not belonging to any of the sets
#' @param highlight which regions of the diagram to highlight
#' @param active_color color for highlight
#' @param inactive_color color for lack of highlight
#' @param scale the base scale (default=`scale_color_manual()`)
#' @inheritDotParams ggplot2::scale_color_manual
#' @export
scale_color_venn_mix = function(
    data, sets=NULL, colors=c('red', 'blue', 'green'), na.value='grey40',
    highlight=NULL, active_color='orange', inactive_color='NA', scale=scale_color_manual,
    ...
) {
    values = prepare_colors(data, sets=sets, colors=colors, empty_color=na.value)

    if (!is.null(highlight)) {
        values[!(names(values) %in% highlight)] = inactive_color
        values[highlight] = active_color
    }

    sets = get_sets(data, sets)
    present_sets = data[, sets]
    present_sets = present_sets[!duplicated(present_sets), ]
    present_sets$name = apply(present_sets[, sets], 1, names_of_members)
    present_sets$label = apply(present_sets[, sets], 1, function(members) {
        present = names(members)[members]
        excluding = setdiff(sets, present)
        paste(
            paste(present, collapse = ' \u2229 '),
            ifelse(length(excluding), '\\', ''),
            paste(excluding, collapse = ' \\ ')
        )
    })


    labels = present_sets$label
    names(labels) = present_sets$name

    empty_name = 'Not in any'
    names(empty_name) = NOT_IN_KNOWN_SETS

    scale(
        values=values,
        labels=c(
            labels,
            empty_name
        ),
        ...
    )
}

#' Fill scale for Venn diagram
#'
#' @param na.value value for elements not belonging to any of the known sets
#' @inheritDotParams scale_color_venn_mix
#' @export
scale_fill_venn_mix = function(..., na.value='NA') {
    scale_color_venn_mix(..., scale=scale_fill_manual, na.value=na.value)
}


push_outwards = function(coords, centre_of_mass, mul) {
    original_coords_for_empty = coords[coords$region == NOT_IN_KNOWN_SETS, ]

    coords$x = coords$x - centre_of_mass['x']
    coords$y = coords$y - centre_of_mass['y']
    coords$x = coords$x * mul
    coords$y = coords$y * mul
    coords$x = coords$x + centre_of_mass['x']
    coords$y = coords$y + centre_of_mass['y']
    coords[coords$region == NOT_IN_KNOWN_SETS, ] = original_coords_for_empty

    coords
}


#' Circle for Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param mapping the aesthetics mapping
#' @param sets vector with names of columns representing membership in sets
#' @param radius the radius of the circle
#' @param resolution the resolution of the circle rasterizer
#' @param size width of the outline
#' @param color the color of the outline
#' @inheritDotParams ggplot2::geom_polygon
#' @export
geom_venn_circle = function(data, mapping=aes_(), sets=NULL, radius=1.5, resolution=100, size=0.8, color='black', ...) {
    set_positions = arrange_venn(data, sets=sets, extract_sets=TRUE, outwards_adjust=1)
    mapping = modifyList(mapping, aes(x=x, y=y, group=region))

    sets = get_sets(data, sets)

    if (length(sets) == 4) {
        a = 5
        b = 5
        set_positions$i = c(1, 1, 2, 2)
    } else {
        a = 1
        b = 1
    }

    coords = do.call(rbind, apply(set_positions, 1, function(specs) {
        steps = seq(0, 2 * pi, length.out=resolution)

        df = data.frame(
            x=radius * a * cos(steps),
            y=radius * b * sin(steps)
        )
        if (length(sets) == 4) {
            angle = ifelse(
                as.numeric(specs[['i']]) %% 2,
                -pi/4,
                pi/4
            )
            df$x = df$x * cos(angle) - df$y * sin(angle)
            df$y = df$x * sin(angle) - df$y * cos(angle)
        }
        df$x = df$x + as.numeric(specs[['x']])
        df$y = df$y + as.numeric(specs[['y']])

        df$region = specs[['region']]
        df$size = as.numeric(specs[['size']])
        df
    }))

    args = list(...)

    if (is.null(mapping$fill)) {
        args$fill = NA
    }

    do.call(
        geom_polygon,
        c(
            list(
                data=coords,
                mapping=mapping,
                size=size,
                color=color
            ),
            args
        )
    )
}


#' Label for a region of Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param mapping the aesthetics mapping
#' @param sets vector with names of columns representing membership in sets
#' @param outwards_adjust the multiplier defining the distance from the centre
#' @param fill the fill of the label
#' @param size the text size
#' @param label.size the size of the label outline
#' @inheritDotParams ggplot2::geom_label
#' @export
geom_venn_label_region = function(
    data,
    mapping=aes_(),
    sets=NULL,
    outwards_adjust=1.3,
    fill=alpha('white', 0.85),
    size=5,
    label.size=0,
    ...
) {
    mapping = modifyList(mapping, aes(x=x, y=y))
    geom_label(
        data=arrange_venn(
            data, sets=sets,
            extract_sets=FALSE,
            extract_regions=TRUE,
            outwards_adjust=outwards_adjust
        ),
        mapping,
        fill=fill,
        label.size=label.size,
        size=size,
        ...
    )
}

#' Label for a set of Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param mapping the aesthetics mapping
#' @param sets vector with names of columns representing membership in sets
#' @param outwards_adjust the multiplier defining the distance from the centre
#' @param fill the fill of the label
#' @param size the text size
#' @param label.size the size of the label outline
#' @inheritDotParams ggplot2::geom_label
#' @export
geom_venn_label_set = function(
    data,
    mapping=aes_(),
    sets=NULL,
    outwards_adjust=2.5,
    fill=alpha('white', 0.85),
    size=5,
    label.size=0,
    ...
) {
    mapping = modifyList(mapping, aes(x=x, y=y))
    geom_label(
        data=arrange_venn(
            data, sets=sets,
            extract_sets=TRUE,
            extract_regions=FALSE,
            outwards_adjust=outwards_adjust
        ),
        mapping,
        fill=fill,
        label.size=label.size,
        size=size,
        ...
    )
}


#' Region of Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param mapping the aesthetics mapping
#' @param sets vector with names of columns representing membership in sets
#' @param resolution the resolution of the circle rasterizer
#' @inheritDotParams ggplot2::geom_polygon
#' @export
geom_venn_region = function(
    data,
    mapping=aes_(),
    sets=NULL,
    resolution=250,
    ...
) {
    mapping = modifyList(aes(x=x, y=y, group=region, fill=region), mapping)

    geom_polygon(
        data=approximate_polygon(data, sets=sets, resolution=resolution),
        mapping=mapping,
        ...
    )
}

minmax_polygon = function(data, layout, grid, resolution, size=0.01) {

    coords = do.call(rbind, sapply(unique(grid$region), function(region) {
        region_grid = grid[grid$region == region, ]
        x_space = unique(region_grid$x)
        x_space = x_space[order(x_space)]

        y_space = unique(region_grid$y)
        y_space = y_space[order(y_space)]

        region_coords_min = do.call(rbind, sapply(x_space, function(x) {
            grid_region_at_x = region_grid[region_grid$x == x, ]
            min_y = grid_region_at_x[grid_region_at_x$y == min(grid_region_at_x$y), ]
            min_y$y = min_y$y - size
            min_y
        }, simplify=FALSE))

        region_coords_max = do.call(rbind, sapply(x_space, function(x) {
            grid_region_at_x = region_grid[region_grid$x == x, ]
            max_y = grid_region_at_x[grid_region_at_x$y == max(grid_region_at_x$y), ]
            max_y$y = max_y$y + size
            max_y
        }, simplify=FALSE))

        region_coords_min_y = do.call(rbind, sapply(y_space, function(y) {
            grid_region_at_y = region_grid[region_grid$y == y, ]
            grid_region_at_y[grid_region_at_y$x == min(grid_region_at_y$x), ]
        }, simplify=FALSE))

        region_coords_max_y = do.call(rbind, sapply(y_space, function(y) {
            grid_region_at_y = region_grid[region_grid$y == y, ]
            grid_region_at_y[grid_region_at_y$x == max(grid_region_at_y$x), ]
        }, simplify=FALSE))


        region_coords = rbind(
            region_coords_min,
            region_coords_max[nrow(region_coords_max):1, ]
        )
        region_coords[region_coords$x == min(region_coords$x), 'x'] = min(region_coords$x) - size
        region_coords[region_coords$x == max(region_coords$x), 'x'] = max(region_coords$x) + size
        region_coords
    }, simplify =FALSE))

    coords
}


approximate_polygon = function(data, sets=NULL, radius=1.5, resolution=200) {
    sets = get_sets(data, sets)

    layout = compute_layout(data, sets, radius=radius)

    grid = allocate_slots(layout, grid_size_x=resolution, grid_size_y=resolution, store_coordinates=TRUE)
    grid = grid[, c('region', 'x', 'y', 'i', 'j')]

    coords = minmax_polygon(data, layout, grid, resolution)
    coords
}


compute_layout = function(data, sets=NULL, radius=1.5) {
    sets = get_sets(data, sets)

    # if (length(sets) == 4) {
    #    a = 2
    #    h = sqrt(3) / 2 * a
    #    # A B C D
    #    x_positions = c(-a/2, a/2, -a/2, a/2)
    #    y_positions = c(h/2, h/2, -h/2, -h/2)
    #} else if (length(sets) == 3) {
    if (length(sets) == 3) {
        a = 2
        h = sqrt(3) / 2 * a
        # A B C
        x_positions = c(-a/2, a/2, 0)
        y_positions = c(h/2, h/2, -h/2)
    } else if (length(sets) == 2) {
        # A B
        x_positions = c(-1, 1)
        y_positions = c(0, 0)
    } else {
        stop('Only supports set number <= 3')
    }

    width = (max(x_positions) + radius) - (min(x_positions) - radius)
    height = (max(y_positions) + radius) - (min(y_positions) - radius)

    list(
        'x_positions'=x_positions,
        'y_positions'=y_positions,
        'radius'=radius,
        'width'=width,
        'height'=height,
        'sets'=sets
    )
}


place_equidistantly_from_sets = function(data, layout) {
    sets = layout$sets

    membership = data[sets]

    coords = membership
    coords$region = apply(membership, 1, names_of_members)

    membership = as.data.frame(lapply(membership, as.numeric))

    x_positions = layout$x_positions
    y_positions = layout$y_positions
    width = layout$width
    height = layout$height

    coords$x = apply(membership, 1, function(element) {
        if (sum(element) == 0) {
            return (width/2)
        }
        sum(element * x_positions) / sum(element)
    })
    coords$y = apply(membership, 1, function(element) {
        if (sum(element) == 0) {
            return (-height/2)
        }
        sum(element * y_positions) / sum(element)
    })
    coords
}


allocate_slots = function(layout, grid_size_x, grid_size_y, store_coordinates=FALSE) {
    x_positions = layout$x_positions
    y_positions = layout$y_positions
    width = layout$width
    height = layout$height
    radius = layout$radius
    sets = layout$sets

    grid_membership = as.data.frame(do.call(rbind, sapply(1:grid_size_x, function(i) {
        x = (i/grid_size_x - 0.5) * width

        t(cbind(sapply(1:grid_size_y, function(j) {
            y = (j/grid_size_y - 0.5) * height

            in_sets = ((x_positions - x) ^ 2) + ((y_positions - y) ^ 2) < (radius ^2)
            names(in_sets) = sets

            in_sets
        })))
    }, simplify=FALSE)))

    grid_membership$region = apply(grid_membership, 1, names_of_members)

    if (store_coordinates) {
        x = c()
        y = c()
        # TODO vectorize
        for (i in 1:grid_size_x) {
            y = c(y, 1:grid_size_y)
            x = c(x, rep(i, grid_size_y))
        }
        grid_membership$i = x
        grid_membership$j = y
        grid_membership$x = (x/grid_size_x - 0.5) * width
        grid_membership$y = (y/grid_size_y - 0.5) * height
    }

    grid_membership
}


#' Arrange points for Venn diagram
#'
#' @param data a dataframe including binary columns representing membership in sets
#' @param sets vector with names of columns representing membership in sets
#' @param radius the radius of the circle
#' @param max_iterations the maximal number of iterations
#' @param verbose should debugging notes be printed?
#' @param outwards_adjust the multiplier defining the distance from the centre
#' @param extract_sets should only sets be extracted?
#' @param extract_regions should all unique regions be extracted?
#' @param repeat_in_intersections repeat intersection k times where k is the number of sets it belongs to?
#' @param starting_grid_size the starting size of the grid for placement of elements
#' @export
arrange_venn = function(
    data, sets=NULL, radius=1.5, max_iterations=10, verbose=FALSE,
    outwards_adjust=1.3,
    extract_sets=FALSE,
    extract_regions=FALSE,
    repeat_in_intersections=FALSE,
    starting_grid_size='auto'
) {
    sets = get_sets(data, sets)
    layout = compute_layout(data, sets, radius=radius)

    coords = place_equidistantly_from_sets(data, layout)
    region_sizes = table(coords$region)

    if (extract_sets) {
        only_in_one_set = coords[rowSums(coords[, sets]) == 1, ]
        coords = only_in_one_set[!duplicated(only_in_one_set[, sets]), ]
    }
    if (extract_regions) {
        coords = coords[!duplicated(coords[, sets]), ]
    }

    sphere_centres = coords[!duplicated(coords), ]
    centre_of_mass = colMeans(sphere_centres[sphere_centres$region != NOT_IN_KNOWN_SETS, c('x', 'y')])

    names(region_sizes)[names(region_sizes) == NOT_IN_KNOWN_SETS] = NOT_IN_KNOWN_SETS

    if (extract_sets || extract_regions) {
        coords = push_outwards(coords, centre_of_mass, outwards_adjust)
        coords[coords$region == '', 'region'] = NOT_IN_KNOWN_SETS
        coords$size = as.numeric(region_sizes[coords$region])

        return (coords)
    }

    if (starting_grid_size == 'auto') {
        grid_size = sqrt(nrow(data))
    } else {
        grid_size = starting_grid_size
    }
    large_enough = FALSE

    is_grid_size_enough = function(grid_size_x, grid_size_y) {
        # so I could calculate the area analitically but it is time consuming for n>3
        # lets just check where the grid points are!
        grid_membership = allocate_slots(layout, grid_size_x, grid_size_y)

        grid_slots_by_region = table(grid_membership$region)

        aligned_sizes = grid_slots_by_region[names(region_sizes)]

        is_enough_slots = aligned_sizes >= region_sizes
        is_capturing_all_sets = !any(is.na(is_enough_slots))

        slots_surplus = min(aligned_sizes - region_sizes, na.rm=TRUE)

        slots_surplus_ratio = slots_surplus / region_sizes[[which.min(aligned_sizes - region_sizes)]]

        if (is_capturing_all_sets && all(is_enough_slots)) {

        } else {
            if (verbose) {
                more_slots_required_for = names(is_enough_slots[!is_enough_slots])
                print(paste0(
                    'Not enough slots in grid ', grid_size_x, ' x ', grid_size_y,
                    ' for ', paste(more_slots_required_for, collapse=', ')
                ))
            }
        }
        slots_surplus_ratio

    }
    iteration = 1
    previous_ratios = c()

    while (!large_enough) {

        slots_surplus_ratio = is_grid_size_enough(grid_size_x=grid_size, grid_size_y=grid_size)

        factor = 1 - slots_surplus_ratio / 2

        if (slots_surplus_ratio == 0) {
            large_enough = TRUE
        } else if (slots_surplus_ratio > 0) {
            new_grid_size = as.integer(round(grid_size * factor))

            if (new_grid_size >= grid_size) {
                # reasonable convergence
                large_enough = TRUE
            # prevent infinite loops
            } else if (slots_surplus_ratio < 0.01 || slots_surplus_ratio %in% previous_ratios) {
                large_enough = TRUE
            } else {
                grid_size = new_grid_size
            }
        } else {
            if (iteration > max_iterations) {
                stop(paste(
                    'Could not find grid large enough to accomodate the elements in the smallest set;',
                    'increase max_iterations to proceed'
                ))
            }

            grid_size = as.integer(round(grid_size * factor))

        }
        previous_ratios = c(slots_surplus_ratio, previous_ratios)
        iteration = iteration + 1
    }

    slots = allocate_slots(layout, grid_size_x=grid_size, grid_size_y=grid_size, store_coordinates=TRUE)

    regions = unique(coords$region)

    # rescale with the cente in the centre of mass
    adjusted_sphere_centres = push_outwards(sphere_centres, centre_of_mass, outwards_adjust)

    new_coords = do.call(rbind, lapply(regions, function(region) {
        region_coords = coords[coords$region == region, ]
        region_slots = slots[slots$region == region, ]

        required_slots = nrow(region_coords)

        cetnre = adjusted_sphere_centres[adjusted_sphere_centres$region == region, c('x', 'y')]

        region_slots$distance_to_centre = (region_slots$x - cetnre$x)^2 + (region_slots$y - cetnre$y)^2
        region_slots = region_slots[order(region_slots$distance_to_centre), ]

        region_coords[, c('x', 'y')] = region_slots[
            1:nrow(region_coords),
            c('x', 'y')
        ]

        if (repeat_in_intersections) {
            region_coords = lapply(unlist(strsplit(region, '-', fixed=TRUE)), function(source_set) {
                region_coords$source_set = source_set
                region_coords
            })
            region_coords = do.call(rbind, region_coords)
        }

        region_coords
    }))

    cbind(new_coords, data[match(rownames(new_coords), rownames(data)), setdiff(colnames(data), sets)])
}
