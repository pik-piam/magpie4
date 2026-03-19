# income

Calculates income based on a MAgPIE gdx file

## Usage

``` r
income(
  gdx,
  file = NULL,
  level = "reg",
  per_capita = TRUE,
  type = "ppp",
  after_shock = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- per_capita:

  income per capita or aggregated for the total population

- type:

  ppp for purchase power parity, mer for market exchange rate

- after_shock:

  FALSE is using the exogenous real income, TRUE is using the
  endogeenous real income that takes into account food price change on
  real income

## Value

annual income as MAgPIE object (unit depends on per_capita: US\$2017
MER/cap/yr (TRUE), US\$2017 MER/yr (FALSE))

## Author

Florian Humpenoeder, Benjamin Bodirsky, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- income(gdx)
} # }
```
