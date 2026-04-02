# buildHTML

Converts a folder with markdown files and a corresponding literature
library (if available) to HTML files and creates cross-links between
them.

## Usage

``` r
buildHTML(
  style = "classic",
  folder = "html",
  mdfolder = "markdown",
  literature = "literature.bib",
  citation = "../CITATION.cff",
  supplementary = "images",
  debug = FALSE,
  templatefolder = ".."
)
```

## Arguments

- style:

  visualization style to be used for the creation. Currently available
  styles are "classic" and "ming"

- folder:

  location the HTML files should be written to

- mdfolder:

  path to the markdown folder to be used as source

- literature:

  path to a bibliography, if available (will be ignored if file does not
  exist)

- citation:

  Citation information in citation file format (optional)

- supplementary:

  a vector of files and/or folders required for the conversion (e.g. an
  images subdirectory with figures to be shown in the documents)

- debug:

  logical which switches on/off a debug mode which will return
  additional status updates and keep build files

- templatefolder:

  Folder in which goxygen will search for template files in addition to
  the pre-installed ones.

## Details

Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be
installed on the system.

## See also

[`goxygen`](goxygen.md), [`buildTEX`](buildTEX.md)

## Author

Jan Philipp Dietrich
