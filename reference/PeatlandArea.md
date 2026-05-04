# PeatlandArea

reads peatland area out of a MAgPIE gdx file

## Usage

``` r
PeatlandArea(gdx, file = NULL, level = "cell", sum = TRUE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any aggregation level
  defined in superAggregateX. In addition "climate" for the 3 climate
  regions tropical, temperate and boreal is available.

- sum:

  sum over land types TRUE (default) or FALSE

## Value

Peatland area in Mha

## Details

Intact, degraded and rewettet peatland area

## Author

Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- PeatlandArea(gdx)
  } # }
```
