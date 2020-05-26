# In-Code documentation for GAMS

R package **goxygen**, version **0.34.0**

[![Travis build status](https://travis-ci.com/pik-piam/goxygen.svg?branch=master)](https://travis-ci.com/pik-piam/goxygen) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1411404.svg)](https://doi.org/10.5281/zenodo.1411404) [![codecov](https://codecov.io/gh/pik-piam/goxygen/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/goxygen)

## Purpose and Functionality

A collection of tools which extract a model documentation from GAMS code and comments. In order to use the package you need to install pandoc and pandoc-citeproc first (https://pandoc.org/).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("goxygen")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **goxygen** in publications use:

Dietrich J, Karstens K (2020). _goxygen: In-Code documentation for GAMS_. doi:
10.5281/zenodo.1411404 (URL: https://doi.org/10.5281/zenodo.1411404), R package version
0.34.0, <URL: https://github.com/pik-piam/goxygen>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {goxygen: In-Code documentation for GAMS},
  author = {Jan Philipp Dietrich and Kristine Karstens},
  year = {2020},
  note = {R package version 0.34.0},
  doi = {10.5281/zenodo.1411404},
  url = {https://github.com/pik-piam/goxygen},
}
```

