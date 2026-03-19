# PriceElasticities

Calculates the physical elasticity for food demand

## Usage

``` r
PriceElasticities(
  gdx,
  file = NULL,
  level = "reg",
  calibrated = TRUE,
  products = "kfo"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "iso" ISO country codes, "reg"
  (regional), "glo" (global)

- calibrated:

  if FALSE, the true regression outputs are used, if TRUE the values
  calibrated to the start years are used

- products:

  set of the products for which the elasticity should be estimated.
  Please note that this stills remains an elasticity relative to total
  food expenditure. So its the change in consumption of one good when
  the prices of all products change according to the scenario.

## Value

magpie object with the livestock share in a region or country. Unit is
dimensionsless, but value depends on chosen attribute

## Author

Benjamin Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- PriceElasticities(gdx)
  } # }
```
