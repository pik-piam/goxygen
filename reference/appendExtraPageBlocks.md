# appendExtraPageBlocks

A helper to merge two nested lists describing extra page blocks. The
lists have the page name on the first level and flattened documentation
blocks on the second level. It is ensured that elements for the same
page are grouped in the same list.

## Usage

``` r
appendExtraPageBlocks(blocks, add)
```

## Arguments

- blocks:

  a nested list for extra page blocks per page

- add:

  a seccond nested list for extra page blocks per page to be appended to
  the first one

## Author

Falk Benke
