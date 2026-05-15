# GrowingStock

reads woody growing stock out of a MAgPIE gdx file

## Usage

``` r
GrowingStock(gdx, file = NULL, level = "regglo", indicator = "relative")
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

- indicator:

  If the reported numbers are relative (mio m3/ha) or absolute (mio.
  m3). Default is relative.

## Value

Growing stock in m3 per ha

## Details

Growing stock for producing woody materials consist of growing stock
from plantations (forestry), secondary and primary forest as well as
other land (natveg)

## Author

Abhijeet Mishra

## Examples

``` r
if (FALSE) { # \dontrun{
x <- GrowingStock(gdx)
} # }
```
