# MAgPIE outputs R package for MAgPIE version 4.x

## Purpose and Functionality

A package containing the most common output routines for extracting outputs from the MAgPIE model.

## Installation

For installation of the most recent package version an additional repository can be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magpie4")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.

## Citation

[![DOI](https://zenodo.org/badge/117547243.svg)](https://zenodo.org/badge/latestdoi/117547243)


