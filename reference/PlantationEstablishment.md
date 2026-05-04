# PlantationEstablishment

reads carbon stocks in harvested timber out of a MAgPIE gdx file

## Usage

``` r
PlantationEstablishment(gdx, file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global), custom regation aggregation
  or any secdforest aggregation level defined in superAggregateX

## Value

Area newly for timber production

## Details

Area newly established in current time step for future timber production

## Author

Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- PlantationEstablishment(gdx)
  } # }
```
