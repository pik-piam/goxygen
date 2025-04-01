# In-Code Documentation for 'GAMS'

R package **goxygen**, version **1.4.5**

[![CRAN status](https://www.r-pkg.org/badges/version/goxygen)](https://cran.r-project.org/package=goxygen) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1411404.svg)](https://doi.org/10.5281/zenodo.1411404) [![R build status](https://github.com/pik-piam/goxygen/workflows/check/badge.svg)](https://github.com/pik-piam/goxygen/actions) [![codecov](https://codecov.io/gh/pik-piam/goxygen/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/goxygen) [![r-universe](https://pik-piam.r-universe.dev/badges/goxygen)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of tools which extract a model documentation from 'GAMS' code and comments. 
             In order to use the package you need to install 'pandoc' and 'pandoc-citeproc' 
             first (<https://pandoc.org/>).


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

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("goxygen") # Creating GAMS model documentations with goxygen
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **goxygen** in publications use:

Dietrich J, Karstens K, Klein D, Baumstark L, Benke F (2025). "goxygen: In-Code Documentation for 'GAMS'." doi:10.5281/zenodo.1411404 <https://doi.org/10.5281/zenodo.1411404>, Version: 1.4.5, <https://github.com/pik-piam/goxygen>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {goxygen: In-Code Documentation for 'GAMS'},
  author = {Jan Philipp Dietrich and Kristine Karstens and David Klein and Lavinia Baumstark and Falk Benke},
  doi = {10.5281/zenodo.1411404},
  date = {2025-04-01},
  year = {2025},
  url = {https://github.com/pik-piam/goxygen},
  note = {Version: 1.4.5},
}
```
