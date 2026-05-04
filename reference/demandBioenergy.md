# demandBioenergy

reads bioenergy demand from a MAgPIE gdx file

## Usage

``` r
demandBioenergy(gdx, file = NULL, level = "reg", sum = FALSE, round = NULL)
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

- sum:

  1st and 2nd generation bioenergy demand (FALSE) or total bioenergy
  demand (TRUE)

- round:

  NULL or number of digits

## Value

A MAgPIE object containing bioenergy demand in EJ/yr

## Author

Jan Philipp Dietrich, Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- demandBioenergy(gdx)
  } # }
```
