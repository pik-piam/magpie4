# Timber

reads timber demand out of a MAgPIE gdx file

## Usage

``` r
Timber(gdx, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "regglo"
  (regional and global), custom region aggregation or any secdforest
  aggregation level defined in superAggregateX

## Value

Forest demandfor timber production

## Details

Forest demandfor timber production

## Author

Abhijeet Mishra

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- Timber(gdx)
  } # }
```
