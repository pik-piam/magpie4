# emisCO2

reads detailed CO2 emissions out of a MAgPIE gdx file

## Usage

``` r
emisCO2(
  gdx,
  file = NULL,
  level = "cell",
  unit = "gas",
  sum_cpool = TRUE,
  sum_land = TRUE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregateX

- unit:

  "element" or "gas"; "element": co2_c in Mt C/yr "gas": co2_c Mt CO2/yr

- sum_cpool:

  aggregate carbon pools (TRUE), below ground (soilc) and above ground
  (vegc and litc) will be reported, if FALSE

- sum_land:

  TRUE (default) or FALSE. Sum over land types (TRUE) or report
  land-type specific emissions (FALSE).

## Value

CO2 emissions as MAgPIE object (unit depends on `unit`)

## Author

Florian Humpenoeder, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- emisCO2(gdx)
} # }
```
