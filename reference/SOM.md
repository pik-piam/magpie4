# SOM

Calculates soil organic carbon stock size based on a MAgPIE gdx file

## Usage

``` r
SOM(
  gdx,
  file = NULL,
  type = "stock",
  reference = "actual",
  level = "reg",
  noncrop_aggr = TRUE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- type:

  "stock" (default) for absoulte values, "density" for per hectar values

- reference:

  default is "actual" (cshare in actual carbon stocks). Other option is
  "target" (cshare in target carbon stocks).

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- noncrop_aggr:

  aggregate non cropland types to 'noncropland' (if FALSE all land types
  of pools59 will be reported)

## Value

A MAgPIE object containing som values

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
x <- SOM(gdx)
} # }
```
