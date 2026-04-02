# check_pandoc

Support function which checks pandoc availability and stops with an
error in case that pandoc cannot be found

## Usage

``` r
check_pandoc(error = FALSE)
```

## Arguments

- error:

  boolean indicating whether function should throw an error in case of
  missing pandoc or return a boolean FALSE.

## Value

boolean indicating whether pandoc is available or not.

## Author

Jan Philipp Dietrich
