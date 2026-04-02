# createListModularCode

support function to create documentation of modular GAMS code.

## Usage

``` r
createListModularCode(
  cc,
  interfaces,
  path = ".",
  citation = NULL,
  unitPattern = c("\\(", "\\)"),
  includeCore = FALSE,
  mainfile = "main.gms",
  docfolder = "doc",
  startType = "equations"
)
```

## Arguments

- cc:

  codeCheck information

- interfaces:

  interface information

- path:

  path to the model to be documented

- citation:

  citation data read from a CFF file

- unitPattern:

  pattern that is usedto identify the unit in the description, default
  =c("\\","\\")

- includeCore:

  Boolean whether core should be included or not, default=FALSE

- mainfile:

  main file of the model

- docfolder:

  folder the documentation should be written to relative to model folder

- startType:

  input parameter for [`extractDocumentation`](extractDocumentation.md)
  to be passed when extracting documentation from realizations. Defaults
  to "equations", meaning that documentation in realizations should be
  interpreted as equations documentation, if no identifier is set.

## See also

[`codeCheck`](https://rdrr.io/pkg/gms/man/codeCheck.html)

## Author

Jan Philipp Dietrich
