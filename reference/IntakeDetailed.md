# IntakeDetailed

Calculates detailed or aggregated per-capita kcal intake including
exogenous scenarios

## Usage

``` r
IntakeDetailed(gdx, file = NULL, level = "reg", product_aggr = FALSE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- product_aggr:

  aggregate over products or not (boolean)

## Value

Calories as MAgPIE object (unit: kcal/cap/day)

## Details

Calculation of kcal food intake is possible for both exogenous diet
scenarios and endogenous estimation from food demand model

## Author

Isabelle Weindl

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- IntakeDetailed(gdx)
  } # }
```
