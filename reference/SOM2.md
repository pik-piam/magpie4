# SOM2

Calculates soil organic carbon stock size based on a MAgPIE gdx file
(for threepool realization)

## Usage

``` r
SOM2(gdx, type = "stock", level = "regglo", noncropAggr = TRUE)
```

## Arguments

- gdx:

  GDX file

- type:

  "stock" (default) for absoulte values, "density" for per hectar values

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global)

- noncropAggr:

  aggregate non cropland types to 'noncropland' (if FALSE all land types
  of pools59 will be reported)

## Value

A MAgPIE object containing som values

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- SOM2(gdx)
} # }
```
