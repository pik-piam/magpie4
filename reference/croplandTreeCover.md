# croplandTreeCover

calculates tree cover on cropland (Mha) from a MAgPIE gdx file

## Usage

``` r
croplandTreeCover(gdx, level = "reg", sum_ac = TRUE, debug = FALSE)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level, reg, glo or regglo, cell or grid

- sum_ac:

  sum over age classes TRUE / FALSE

- debug:

  debug mode TRUE makes some consistency checks between estimates for
  different resolutions

## Author

Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- fallow(gdx)
} # }
```
