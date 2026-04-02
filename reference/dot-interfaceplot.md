# .interfaceplot

helper function which includes interface plot figures in a markdown
document, if available. The figures need to be created beforehand.

## Usage

``` r
.interfaceplot(zz, name, docfolder)
```

## Arguments

- zz:

  a connection object of class "textConnection" containing the markdown
  document

- name:

  Name of the module for which the interfaceplot should be shown

- docfolder:

  folder the documentation should be written to relative to model folder

## See also

[`goxygen`](goxygen.md), [`createModulePage`](createModulePage.md)

## Author

Jan Philipp Dietrich
