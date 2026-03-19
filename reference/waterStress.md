# waterStress

calculates which areas are affected by water stress from water
availability and water demand in MAgPIE. Water stress is calculated
based on the proportion of water withdrawals to water availability.
Thresholds based on World Resources Institute definition
(https://www.wri.org/data/water-stress-country): Low stress: \<10
Low-to-medium stress: 10-20 Medium to high stress: 20-40 High stress:
40-80 Extremely high stress: \>80

## Usage

``` r
waterStress(gdx, stressRatio = 0.4, file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- stressRatio:

  threshold defining level of water stress (e.g. 0.2 for medium water
  stress, 0.4 for high water stress)

- file:

  a file name the output should be written to using write.magpie

- level:

  spatial level of aggregation: "cell" (cellular), "reg" (regional),
  "glo" (global), "regglo" (regional and global)

## Value

MAgPIE object indicating whether location is water stressed (1) or not
(0)

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- waterStress(gdx)
} # }
```
