# laborCostsEndo

reads MAgPIE endogenous labor costs for crop and livestock production
from gdx file

## Usage

``` r
laborCostsEndo(gdx, products = "kcr", file = NULL, level = "grid")
```

## Arguments

- gdx:

  GDX file

- products:

  products for which labor costs should be reported ("kcr" or "kli", for
  other products use factorCosts())

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("grid" or "iso", for regional/global
  use factorCosts())

## Value

MAgPIE object containing labor costs \[million US\$17\]

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- laborCostsEndo(gdx)
} # }
```
