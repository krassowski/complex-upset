# Version 0.9.0

2020-12-20

New features:
- Intersection modes were formalized with the default remaining `exclusive_intersection` (alias `distinct`); additional modes are: `inclusive_intersection` (alias `intersect`), `inclusive_union` and `exclusive_union`; please read the relevant part of the documentation for details (#78).
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
