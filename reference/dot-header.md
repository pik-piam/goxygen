# .header

helper function which writes a title for a markdown section

## Usage

``` r
.header(zz, title, level, id = NULL)
```

## Arguments

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

Jan Philipp Dietrich
