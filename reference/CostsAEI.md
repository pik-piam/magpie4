# CostsAEI

reads AEI costs entering the objective function from a MAgPIE gdx file

## Usage

``` r
CostsAEI(gdx, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation ("reg", "glo", "regglo")

## Value

MAgPIE object containing costs for AEI \[million US\$17\]

## Author

Felicitas Beier

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- CostsAEI(gdx)
  } # }
```
