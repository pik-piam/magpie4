# factorCosts

reads factor costs for crops, livestock, residues or pasture entering
the objective function from a MAgPIE gdx file. Depending on the product
and the MAgPIE version (and factor cost realization), factor costs are
either already split into labor and capital, will be split in this
function, or are kept as the aggregate

## Usage

``` r
factorCosts(gdx, products = "kli", file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- products:

  products for which factor costs should be reported ("kcr", "kli",
  "kres", "fish", or "pasture")

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

## Value

MAgPIE object containing factor costs \[million US\$17\]

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- factorCosts(gdx)
} # }
```
