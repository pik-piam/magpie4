# fallow

calculates fallow land (Mha) from a MAgPIE gdx file

## Usage

``` r
fallow(gdx, level = "reg", debug = FALSE)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level, reg, glo or regglo, cell or grid

- debug:

  debug mode TRUE makes some consistency checks between estimates for
  different resolutions

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- fallow(gdx)
} # }
```
