# CostsFertilizer

reads costs entering the objective function from a MAgPIE gdx file

## Usage

``` r
CostsFertilizer(gdx, file = NULL, level = "regglo", disagg = TRUE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

- disagg:

  whether costs should be disaggregated into the different crop types

## Value

MAgPIE object containing fertilizer costs \[million US\$17\]

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- CostsFertilizer(gdx)
  } # }
```
