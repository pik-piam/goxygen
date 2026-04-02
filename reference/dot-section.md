# .section

helper function which creates a section consisting of header and content
in a markdown document and skips section when content is empty

## Usage

``` r
.section(data, zz, title, level, id = NULL)
```

## Arguments

- data:

  a character vector to be written to the markdown document

- zz:

  a connection object of class "textConnection" containing the markdown
  document

- title:

  the title to be used (character vector of length 1)

- level:

  level of the heading (1 means main header, higher numbers reflect
  lower levels)

- id:

  ID given to the title (relevant for anchors)

## See also

[`goxygen`](goxygen.md), [`createModulePage`](createModulePage.md)

## Author

Falk Benke
