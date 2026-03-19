# feed

calculates feed demand by animal type out of a gdx file

## Usage

``` r
feed(
  gdx,
  file = NULL,
  level = "reg",
  detail = TRUE,
  nutrient = "dm",
  balanceflow = TRUE
)
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

- detail:

  if FALSE, only total feed demand per animal type is calculated without
  details on the type of feed

- nutrient:

  The nutrient in which the results shall be calculated

- balanceflow:

  If true, feed includes the calibration balanceflow

## Value

feed demand by animal type as MAgPIE object (unit depends on selected
nutrient attributes)

## Author

Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- feed(gdx)
  } # }
```
