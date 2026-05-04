# PriceGHG

reads GHG emission prices out of a MAgPIE gdx file

## Usage

``` r
PriceGHG(gdx, file = NULL, level = "reg", aggr = "weight")
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

- aggr:

  aggregation used, currently only "weight" (weighted by population)
  (max is deprecated)

## Value

GHG emission prices as MAgPIE object (US\$2017/tCO2, US\$2017/tN2O,
US\$2017/tCH4)

## See also

[`reportPriceGHG`](reportPriceGHG.md)

## Author

Florian Humpenoeder, Amsalu W. Yalew

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- PriceGHG(gdx)
  } # }
```
