## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* It depends on two packages (citation, gms) which just have been uploaded to CRAN
* All hints received during the upload of citation and gms have been checked for this package as well, in particular:
** the DESCRIPTION has been updated according to the styling guidelines
** it has been made sure that tests, vignettes and examples only write or modify files in a temporary directory (tempdir())
** when files are written, file paths can be modified by the user to determine where the files should be written to
