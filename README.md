# MAgPIE outputs R package for MAgPIE version 4.x

R package **magpie4**, version **2.39.2**

[![CRAN status](https://www.r-pkg.org/badges/version/magpie4)](https://cran.r-project.org/package=magpie4) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158582.svg)](https://doi.org/10.5281/zenodo.1158582) [![R build status](https://github.com/pik-piam/magpie4/workflows/check/badge.svg)](https://github.com/pik-piam/magpie4/actions) [![codecov](https://codecov.io/gh/pik-piam/magpie4/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/magpie4) [![r-universe](https://pik-piam.r-universe.dev/badges/magpie4)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Common output routines for extracting results from the MAgPIE
    framework (versions 4.x).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magpie4")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **magpie4** in publications use:

Bodirsky B, Humpenoeder F, Dietrich J, Stevanovic M, Weindl I, Karstens K, Wang X, Mishra A, Beier F, Breier J, Yalew A, Chen D, Biewald A, Wirth S, von Jeetze P, Leip D, Crawford M, Alves M, Sauer P, Bonsch M, Vartika S (2025). "magpie4: MAgPIE outputs R package for MAgPIE version 4.x." doi:10.5281/zenodo.1158582 <https://doi.org/10.5281/zenodo.1158582>, Version: 2.39.2, <https://github.com/pik-piam/magpie4>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {magpie4: MAgPIE outputs R package for MAgPIE version 4.x},
  author = {Benjamin Leon Bodirsky and Florian Humpenoeder and Jan Philipp Dietrich and Miodrag Stevanovic and Isabelle Weindl and Kristine Karstens and Xiaoxi Wang and Abhijeet Mishra and Felicitas Beier and Jannes Breier and Amsalu Woldie Yalew and David Chen and Anne Biewald and Stephen Wirth and Patrick {von Jeetze} and Debbora Leip and Michael Crawford and Marcos Alves and Pascal Sauer and Markus Bonsch and Singh Vartika},
  doi = {10.5281/zenodo.1158582},
  date = {2025-09-18},
  year = {2025},
  url = {https://github.com/pik-piam/magpie4},
  note = {Version: 2.39.2},
}
```
