# gamsequation2tex

Convert a gams equation into latex code

## Usage

``` r
gamsequation2tex(x)
```

## Arguments

- x:

  GAMS equation provided as character

## Value

GAMS equation converted to latex code

## See also

[`goxygen`](goxygen.md)

## Author

Jan Philipp Dietrich

## Examples

``` r
  x <- "eq_1 .. v_a =e= sum(j,v_b(j)*((1-s_c)+sum(cell(i,j),v_d(i)/f_d(i))));"
  cat(gamsequation2tex(x))
#> \begin{multline*}
#>  v\_a = \sum_{j}\left(v\_b(j) \cdot \left(\left(1-s\_c\right)+\sum_{cell(i,j)}\left(\frac{v\_d(i)}{f\_d(i)}\right)\right)\right) 
#> \end{multline*}
```
