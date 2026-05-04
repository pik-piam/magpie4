# RotationLength

reads rotation length out of a MAgPIE gdx file

## Usage

``` r
RotationLength(gdx, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any secdforest aggregation
  level defined in superAggregate

## Value

Forest rotation length

## Details

Forest rotation length

## Author

Abhijeet Mishra

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- RotationLength(gdx)
  } # }
```
