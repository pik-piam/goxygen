# buildTEX

Converts a folder with markdown files and a corresponding literature
library (if available) to a tex file

## Usage

``` r
buildTEX(
  file = "documentation.tex",
  mdfolder = "markdown",
  literature = "literature.bib",
  citation = "../CITATION.cff",
  supplementary = NULL,
  pdf = TRUE,
  style = "classic",
  templatefolder = ".."
)
```

## Arguments

- file:

  name of the tex file to be written

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

- pdf:

  boolean which specifies whether pdf file should be generated from tex

- style:

  visualization style to be used for the Latex/PDF creation. Currently
  only "classic" style is available. Ignored for outputs other than
  Latex/PDF. Can be extended by additional templates stored in the
  `templatefolder` in the format `<style>.latex`. Classic template
  `system.file("templates","classic.latex",package="goxygen")` can serve
  as a starting point for own templates.

- templatefolder:

  Folder in which goxygen will search for template files in addition to
  the pre-installed ones.

## Details

Pandoc (https://pandoc.org/) together with pandoc-citeproc need to be
installed on the system.

## See also

[`goxygen`](goxygen.md), [`buildHTML`](buildHTML.md)

## Author

Jan Philipp Dietrich, Kristine Karstens
