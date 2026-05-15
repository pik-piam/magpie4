# waterStressedPopulation

People living in water stressed region

## Usage

``` r
waterStressedPopulation(gdx, file = NULL, level = "cell", absolute = TRUE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global) or "grid" (grid cell)

- absolute:

  TRUE: reports people living in water stressed region in million,
  FALSE: returns share of population

## Value

MAgPIE object

## Author

Felicitas Beier

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- waterStressRatio(gdx)
  } # }
```
