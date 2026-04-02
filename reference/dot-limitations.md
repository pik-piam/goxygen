# .limitations

helper function which adds a "limitations" section.

## Usage

``` r
.limitations(zz, limitations, emptyIfNULL = FALSE)
```

## Arguments

- zz:

  a connection object of class "textConnection" containing the markdown
  document

- limitations:

  A character vector containing the given limitations

- emptyIfNULL:

  switch which decides whether limitations section should be ignored, if
  limitations input is NULL or if it should state that there are no
  known limitations.

## See also

[`goxygen`](goxygen.md), [`createModulePage`](createModulePage.md)

## Author

Jan Philipp Dietrich
