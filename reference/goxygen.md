# goxygen

Documentation function which extracts a full model documentation from a
modularized gams model. The function extracts comments used as
documentation, extracts code and can extract and convert GAMS equations
as latex code. Output is returned in Markdown, HTML and PDF format.

## Usage

``` r
goxygen(
  path = ".",
  docfolder = "doc",
  cache = FALSE,
  output = c("html", "tex", "pdf"),
  htmlStyle = "ming",
  texStyle = "classic",
  templatefolder = ".",
  cff = "CITATION.cff",
  modularCode = is.modularGAMS(),
  unitPattern = c("\\(", "\\)"),
  includeCore = FALSE,
  mainfile = "main.gms",
  startType = "equations",
  ...
)
```

## Arguments

- path:

  path to the model to be documented

- docfolder:

  folder the documentation should be written to relative to model folder

- cache:

  Boolean to allow read data from existing cache file

- output:

  List of output to be written, available are "html","pdf" and "tex"

- htmlStyle:

  visualization style to be used for the HTML creation. Currently
  available styles are "classic" and "ming". Ignored for outputs other
  than HTML. Can be extended by additional templates stored in the
  `templatefolder` in the form `<style>.html5` together with a subfolder
  with supplementary files and the name of the style `<style>` (both
  need to be provided). The preinstalled ming template
  `system.file("templates","ming.css",package="goxygen")` and
  `system.file("templates","ming.html5",package="goxygen")` can serve as
  a starting point for own templates.

- texStyle:

  visualization style to be used for the Latex/PDF creation. Currently
  only "classic" style is available. Ignored for outputs other than
  Latex/PDF. Can be extended by additional templates stored in the
  `templatefolder` in the format `<style>.latex`. Classic template
  `system.file("templates","classic.latex",package="goxygen")` can serve
  as a starting point for own templates.

- templatefolder:

  Folder in which goxygen will search for template files in addition to
  the pre-installed ones.

- cff:

  path to a citation file in citation-file-format (ignored if not
  existing)

- modularCode:

  Boolean deciding whether code should be interpreted as modular GAMS
  code (only av)

- unitPattern:

  pattern that is usedto identify the unit in the description, default
  =c("\\","\\")

- includeCore:

  boolean whether core should be included or not, default=FALSE

- mainfile:

  main file of the model

- startType:

  input parameter for
  [`createListModularCode`](createListModularCode.md), default =
  "equations"

- ...:

  optional arguments to
  [`interfaceplot`](https://rdrr.io/pkg/gms/man/interfaceplot.html),
  passed via
  [`modules_interfaceplot`](https://rdrr.io/pkg/gms/man/modules_interfaceplot.html).

## Note

Documentation lines in the code must start with \*' to be detected as
documentation. Identifier at the beginning of each block describe what
kind of documentation is given. All identifiers start with @ followed by
the name of the identifier. Currently, following identifiers are
available

- @title Title

- @authors List of authors

- @description Model description (only the documentation text will be
  interpreted)

- @equations Equation description (documentation text will be extracted
  and gams equations will be converted to latex code)

- @code Code description (documentation text and code will be extracted)

- @limitations details about limitations of an implementation

- @stop everything following will be ignored until the next identifier
  is mentioned again. Useful to stop a section

In addition you can store a model logo (100px height, 100px weight) as
`logo.png` in the main folder of the model which then will be used in
the HTML version of the documentation. If you want to add citations to
your documentation you can do so by adding a bibtex file with the name
literature.bib in the main folder of the model. To link these references
in the text you can use the syntax `@<id>` in which "\<id\>" stands for
the identifier given to the corresponding bibtex entry.

## See also

[`codeCheck`](https://rdrr.io/pkg/gms/man/codeCheck.html),[`interfaceplot`](https://rdrr.io/pkg/gms/man/interfaceplot.html)

## Author

Jan Philipp Dietrich

## Examples

``` r
# make sure that pandoc is available
if (check_pandoc()) {
  # run goxygen for dummy model and store documentation as HTML in a temporary directory
  docfolder <- paste0(tempdir(), "/doc")
  goxygen(system.file("dummymodel", package = "gms"),  docfolder = docfolder, output = "html")
}
#> 
#>  Running codeCheck...
#>  Finished data collection...            (time elapsed:  0.009)
#>  Naming conventions check done...       (time elapsed:  0.009)
#>   Running checkAppearance...
#>   Start variable matching...            (time elapsed:  0.001)
#>   Finished variable matching...         (time elapsed:  0.001)
#>   Start var capitalization check...     (time elapsed:  0.002)
#>   Finished var capitalization check...  (time elapsed:  0.003)
#>  Investigated variable appearances...   (time elapsed:  0.013)
#>  Appearance and usage check done...     (time elapsed:  0.014)
#>  Switch Appearance check done...        (time elapsed:  0.015)
#>  Interface collection and check done... (time elapsed:  0.015)
#>  Input folder check done...             (time elapsed:  0.016)
#>  Description check done...              (time elapsed:  0.016)
#>  All codeCheck tests passed!
#> Warning: The package qgraph is required for creating interface plots!
#> Warning: The package qgraph is required for creating interface plots!
#> Warning: The package qgraph is required for creating interface plots!
#> Warning: Nothing to plot anymore after filters have been applied!
#> Start HTML creation...
#> ...finished HTML creation!
```
