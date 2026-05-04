# IntakeDetailedProtein

Calculates food-specific per-capita protein intake from magpie results
in grams.

## Usage

``` r
IntakeDetailedProtein(gdx, file = NULL, level = "reg", product_aggr = FALSE)
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

Protein intake as MAgPIE object (unit: grams/cap/day)

## Author

Vartika Singh, Isabelle Weindl

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- IntakeDetailedProtein(gdx)
  } # }
```
