# Version 0.7.4

2020-12-06

- significant performance improvements, especially for large datasets and `group_by` (#12)
- set sizes can now be moved to the right side of the plot using `upset_set_size(position='right')`
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
