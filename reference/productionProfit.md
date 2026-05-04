# productionProfit

calcluates aggregate producer profit based on a MAgPIE gdx file.

## Usage

``` r
productionProfit(gdx, file = NULL, level = "reg")
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

## Value

A MAgPIE object containing producers profit in million \$US.

## Author

Miodrag Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- productionProfit(gdx)
  } # }
```
