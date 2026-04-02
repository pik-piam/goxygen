# createModulePage

Creates markdown code from a supplied data list

## Usage

``` r
createModulePage(data, docfolder)
```

## Arguments

- data:

  a list of data entries for the resulting markdown page. Following
  entries can be provided:

  name

  :   Name of the module

  title

  :   Page title

  description

  :   General description

  input

  :   Table containing inputs to the module

  output

  :   Table containing outputs from the module

  realizations

  :   A list of realizations with entries "description" and
      "limitations" for each of them

  declarations

  :   Table of declarations for internal objects

  stes

  :   Table containing sets used in the module

  authors

  :   Module authors

  seealso

  :   A vector with names of relevant other documentation pages.

- docfolder:

  folder the documentation should be written to relative to model folder

## Value

a character vector containing the produced markdown text

## See also

[`goxygen`](goxygen.md)

## Author

Jan Philipp Dietrich
