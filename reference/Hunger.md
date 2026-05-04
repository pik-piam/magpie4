# Hunger

Calculates the share of people living in hunger.

## Usage

``` r
Hunger(
  gdx,
  level = "reg",
  after_shock = TRUE,
  calibrated = FALSE,
  share = TRUE
)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation. can be "iso","reg","regglo","glo"

- after_shock:

  FALSE is using the exogenous real income and the prices before a
  shock, TRUE is using the endogeenous real income that takes into
  account food price change on real income

- calibrated:

  if calibrated is TRUE, kcal values are calibrated to better match
  historical years

- share:

  share of population that is undernourished

## Value

magpie object with hunger (mio people) or hunger share

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- Hunger(gdx)
  } # }
```
