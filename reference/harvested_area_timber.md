# harvested_area_timber

Reads wood harvest area separated by source (primforest, secdforest,
forestry, other) and age classes from a gdx. The data is on cluster
level and the unit is Mha per year.

## Usage

``` r
harvested_area_timber(
  gdx,
  file = NULL,
  level = "cell",
  aggregateAgeClasses = TRUE,
  annualized = TRUE
)
```

## Arguments

- gdx:

  A fulldata.gdx of a magpie run, usually with endogenous forestry
  enabled

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any secdforest aggregation
  level defined in superAggregate

- aggregateAgeClasses:

  If TRUE, age classes are aggregated

- annualized:

  If TRUE, Mha per year. If FALSE, Mha per time step

## Value

Area harvested for wood in Mha per year (annualized = TRUE) or Mha per
time step (annualized = FALSE) as a magpie object

## Author

Abhijeet Mishra, Pascal Sauer, Florian Humpenoeder
