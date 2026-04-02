# flattenPageBlockList

A helper that processes additional attributes for a given list of code
documentation blocks. Code documentation blocks are described as lists
consisting of \`content\` containing the documentation and a \`cfg\`
list containing attributes.

## Usage

``` r
flattenPageBlockList(data)
```

## Arguments

- data:

  a list of documentation pieces with type as name of each element

## Value

a list with two element (1) \`blocks\` containing the documentation
elements with type as name of the element and (2) \`extraPageBlocks\`
containing lists for blocks to be put on an extra pages, sorted by page
names.

## Details

If a block entry has the \`cgf\` attribute \`extrapage\`, it is moved to
a separate list \`extraPageBlocks\` in the output, as these need to be
rendered separately later.

Regular blocks without the \`extrapage\` attribute are moved to a list
\`blocks\` and multiple blocks with the same name are merged into one
block.

Cfg attributes other than \`extrapage\` are currently not supported and
therefore ignored, but a warning is thrown.

After processing the \`cfg\` attributes, the code documentation blocks
are flattened, i.e. a list consisting of a \`content\` and \`cfg\` entry
is replaced by the data in \`cfg\`.

This helper supports nesting of blocks in \`realizations\` with code
documentation per realization.

## Author

Falk Benke
