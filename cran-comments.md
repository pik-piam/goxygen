## Test environments
* local R installation, R 3.6.3
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a bugfix release.
* The package had previously been uploaded and released on CRAN but had been removed due to an issue on one of the windows servers which did not had pandoc installed. Due to vacation I was not able to fix the issue in time.
* The issue has now been fixed via disabling pandoc-depending code on systems which do not have pandoc available.