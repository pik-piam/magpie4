# carbonLTS

reads carbon stored in harvested timber out of a MAgPIE gdx file

## Usage

``` r
carbonLTS(
  gdx,
  file = NULL,
  level = "cell",
  unit = "element",
  cumulative = FALSE,
  baseyear = 1995
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "cell", "reg" (regional), "glo"
  (global), "regglo" (regional and global) or any secdforest aggregation
  level defined in superAggregateX

- unit:

  element" or "gas"; "element": co2_c in Mt C/yr, n2o_n in Mt N/yr, ch4
  in Mt CH4/yr; "gas": co2_c Mt CO2/yr, n2o_n in Mt NO2/yr, ch4 in Mt
  CH4/yr

- cumulative:

  Logical; Determines if cHWP emissions are reported annually (FALSE) or
  cumulative (TRUE). The starting point for cumulative emissions is
  y1995.

- baseyear:

  Baseyear used for cumulative emissions (default = 1995)

## Value

carbon stocks in MtC from harvested timber

## Details

Annual (and cumulative) Carbon stored in harvested wood products, as
well as, slow emissions from half life deacy.

## Author

Abhijeet Mishra, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- carbonLTS(gdx)
} # }
```
