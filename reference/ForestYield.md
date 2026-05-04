# ForestYield

Calculates the average growing stock density of harvested forest areas
(m3/ha). This is the ratio of timber production volume to harvested
area, i.e. the average standing volume removed per hectare harvested.
Values are comparable to growing stock (typically 100-300 m3/ha) and
should NOT be confused with forestry yield in the MAI sense (5-20
m3/ha/yr).

## Usage

``` r
ForestYield(gdx, file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any secdforest aggregation
  level defined in superAggregate

## Value

Average growing stock density of harvested areas in m3/ha

## Details

Average growing stock of harvested areas = timber production (m3) /
harvested area (ha). Differences to overall growing stock arise from
selective harvesting of specific age classes.

## Author

Abhijeet Mishra, Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- ForestYield(gdx)
  } # }
```
