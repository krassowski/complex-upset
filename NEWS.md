# Version 1.2.0

Unreleased

Major improvements:
- manually specified intersections will now display empty intersections and non-exclusive intersections correctly #109
- manually specified intersections do not require modifying the `intersect` argument to obtain the intended result any longer #109

Minor improvements:
- data.table can be passed instead of data.frame (the conversion will be performed atuomatically) #105
- warning will be shown if a vecotor is provided instead of a list to the `intersections` argument #109
- when `intersections` argument includes sets not specified in `intersect`, a warning will be issued and execution will proceed as if those were included in `intersect` #109
- when incorrect names of sets are passed to `intersections` argument, those will be highlighted and plotting will be aborted #109

# Version 1.1.0

2021-01-13

New features:
- it is now possible to select specific intersections passing `intersections = list('Drama', c('Comedy', 'Romance'))`
- it is now possible to define custom order of intersections passing `intersections = list('Drama', c('Comedy', 'Romance'))` and `sort_intersections = FALSE`

# Version 1.0.3

2021-01-05

Bug fixes:
- Major performance and memory use improvements, especially when using `intersections = 'all'`
- The `max_combinations_n` fail-safe was replaced by a more useful `max_combinations_datapoints_n` with more precise error message
- The atypical use case of filtering with `max_degree = 0` is now accepted again
- Fix tests using all the data (effectively union mode) rather than chosen mode data only

# Version 1.0.2

2021-01-04

Bug fixes:
- Fixed regression of non-observed sets causing "no vector columns were selected" caused by fix addressing #90
- Reduced length of file names for some test doppelgangers

# Version 1.0.1

2021-01-04

Bug fixes:
- Filtering by degree when using non-default mode and `intersections='all'` now correctly accounts for all observations (#89)
- Empty sets/groups are now correctly removed when filtering with a non-default mode (#90)
- Missing values are now converted to FALSE and a warning is issued to the user rather than causing an undefined behavior (#88)

# Version 1.0.0

2020-12-30

Changes:
- Lists with computed sizes are now returned in a single list called `sizes` by `upset_data()`
- Set sizes are now **not** filtered by default when selecting intersections by `min_*` or `max_*` criteria. Pass `filter_intersections=TRUE` to `upset_set_sizes()` to restore the previous behavior
- Union sizes are now calculated before data trimming which provides more accurate ratio estimates
- Added examples for Venn diagrams which are now covered by automated tests to protect against regressions
- Removed `upset_data()` `intersected` member to avoid needless duplication of the data frames; access `with_sizes` instead
- `aest` argument of `intersection_size()` and related functions was renamed to `mapping` and is now the first positional argument
- `min_max_early` argument is no longer required and was removed

New features:
- Annotations can now access data for any of the available modes by adding `upset_mode()` layer. By default the annotations are given data corresponding to the same mode as the mode of the passed in the `upset()` call.
- It is now possible to display all intersections, even if those are not present in the data by passing `intersections='all'` to `upset()`; this is only feasible for <20 sets, but filtering by degree can allow to explore a subset of all intersections when there are many more sets; this is only useful for modes different from the default exclusive intersection.
- If filtering leads to no intersections, an informative error is shown (#80)


Bug fixes:
- Modes passed to `upset()` are now also used for sorting and trimming
- Size calculation for modes was optimized for better performance
- User-added layers are now shown on top of `intersection_size()` and `intersection_union()`
- Column names are no longer modified when supplying to `ggplot2` allowing to easily use them in annotations (#82)

# Version 0.9.1

2020-12-20

Changes:
- Pass metadata to the `ggplot2` when arranging Venn diagram, allowing to map elements aesthetics details

# Version 0.9.0

2020-12-20

New features:
- Intersection modes were formalized with the default remaining `exclusive_intersection` (alias `distinct`); additional modes are: `inclusive_intersection` (alias `intersect`), `inclusive_union` and `exclusive_union`; please read the [relevant part of the documentation](https://krassowski.github.io/complex-upset/articles/Examples_R.html#0-2-region-selection-modes) for details (#78).
- Simple Venn diagrams (for two or three sets) can now be constructed using same input (binary presence data frame) using pseudo geoms: `geom_venn_circle()`, `geom_venn_label_region()`, `geom_venn_label_set()`, `geom_venn_region()` and scales `scale_color_venn_mix()` and `scale_fill_venn_mix()`; while developed mostly for the documentation needs, it provides unique capability of highlighting relevant regions of the Venn diagram and placing observations within appropriate regions (which allows to demonstrate their attributes with appropriate aesthetics mapping).

Changes:
- Breaking: union size for "empty" intersection is now equal to its size

Bug fixes:
- Layers added to `upset_set_size()` and `intersection_matrix()` will now always go on top (avoiding geoms being hidden underneath)
- Declare layer in NAMESPACE to allow basic usage without loading `ggplot2`
- `upset_query()` will now throw an informative error when the user forgets to pass any aesthetics (#79)

# Version 0.8.0

2020-12-06

- Breaking: `geom` of `upset_set_size()` now accepts geom object (e.g. `geom_bar()`) rather than a function (e.g. `geom_bar`)
- Breaking: `upset_set_size()` no longer accepts variadic arguments (`...`); please modify the `geom` instead
- Additional geoms added to `upset_set_size()` are now added on top of stripes, thus properly visible
- Data available for `upset_set_size()` now includes all metadata of the original data frame, enabling to annotate the bars, e.g.:
   - `upset_set_size(geom=geom_bar(aes(fill=mpaa, x=group)))`
- Intersections can now be sorted by multiple criteria, e.g. first by degree and then by cardinality: `sort_intersections_by=c('degree', 'cardinality')` (#47)
- Breaking: `dot_size` argument was removed. Use `matrix=intersection_matrix(geom=geom_point(size=5))` instead
- Intersection matrix can now be customized, including the points (e.g. changing shape to squares), segments (e.g. using dotted line) and outlines (changing color) allowing to create [Example 5.4](https://krassowski.github.io/complex-upset/articles/Examples_R.html):
 > ![](https://raw.githubusercontent.com/krassowski/complex-upset/master/tests/figs/examples/example-5-4-adjusting-the-intersection-matrix-1.svg)

# Version 0.7.4

2020-12-06

- Significant performance improvements, especially for large datasets and `group_by` (#12)
- Set sizes can now be moved to the right side of the plot using `upset_set_size(position='right')`
- `guides='over'` can be now passed to `upset()` to place the legends over the set sizes

# Version 0.7.3

2020-12-05

- Address CRAN review comments
- Add CITATION file
- Add examples of the use in academic papers to README
- Compress images in documentation for faster loading

# Version 0.7.2

2020-12-04

- Prepare for CRAN submission
- Fix various typos
- Add more examples to documentation (#44, #38)

# Version 0.7.1

2020-12-04

- Fix degree calculation for empty intersections (#73)

# Version 0.7.0

2020-12-04

- Refactor `upset_set_size()` to enable addition of ggplot2 objects with `+`.
- Add missing documentation for `aes_percentage()`

# Version 0.6.3

2020-12-04

- Move to GitHub CI
- Use warnings rather than `print()` (#53)
- Eliminate unwanted warnings and messages

# Version 0.6.3

2020-12-03

Including versions 0.6.0 - 0.6.2.

- Fix display of stripes in set size component
- Refactor annotations of intersections to allow adding ggplot2 objects with `+` (#67, #27, #61)
- Add `n_intersections` parameter (#70)

# Version 0.5.19

2020-11-08

- Implement `group_by` argument (#66)

# Version 0.5.18

2020-09-21

- Multiple queries now work correctly (#60)

# Version 0.5.17

2020-09-11

- Fix `min_degree` not working with intersections of size one (#48)

# Version 0.5.16

2020-09-11

- Refactor stripes implementation, allow stripes transparency (#54)

# Version 0.5.15

2020-06-18

- Fix the counts not showing up on highlighted bars (#43)

# Version 0.4.0

- Sorting (descending/ascending/none) by degree/cardinality/ratio (#18, #13, #10)
- Selective themes customization (#13)
- Set size customization including rotations, log, ticks and more (#11)
- Added roxygen-based documentation and reference
