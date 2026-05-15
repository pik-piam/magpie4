# OtherLand

Disaggregation of other land into initial, restored and recovered land
based on a MAgPIE gdx file

## Usage

``` r
OtherLand(gdx, level = "reg")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any aggregation level
  defined in superAggregateX. In addition "climate" for the 3 climate
  regions tropical, temperate and boreal is available.

## Value

Other land area in Mha

## Details

initial, restored and recovered land

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- PeatlandArea(gdx)
  } # }
```
