# LivestockShare

Calculates the livestock share from the food demand model

## Usage

``` r
LivestockShare(
  gdx,
  file = NULL,
  level = "reg",
  after_shock = TRUE,
  calibrated = TRUE,
  attributes = "kcal",
  fish = TRUE
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

- after_shock:

  FALSE is using the exogenous real income and the prices before a
  shock, TRUE is using the endogeenous real income that takes into
  account food price change on real income

- calibrated:

  if FALSE, the true regression outputs are used, if TRUE the values
  calibrated to the start years are used

- attributes:

  unit: kilocalories per day ("kcal"), g protein per day ("protein"). Mt
  reactive nitrogen ("nr").

- fish:

  if true, livestock share includes fish, otherwhise not

## Value

magpie object with the livestock share in a region or country. Unit is
dimensionsless, but value depends on chosen attribute

## Author

Benjamin Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- LivestockShare(gdx)
  } # }
```
