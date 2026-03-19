# surplusChange

calculates aggregate change in economic surplus in mio.US\$ based on a
MAgPIE gdx files from two different scenarios.

## Usage

``` r
surplusChange(gdx1, gdx2, file = NULL, level = "reg", type = "consumer")
```

## Arguments

- gdx1:

  GDX file from benchmark scenario

- gdx2:

  GDX file from the analyzed scenario

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- type:

  Economic surplus type: "consumer" (default), "producer" or "welfare"

## Value

A MAgPIE object containing aggregate changes in producer surplus,
consumer surplus and aggregate economic welfare between an analyzed
scenario and a benchmark scenario, in million \$US.

## Author

Miodrag Stevanovic

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- surplusChange(gdx1, gdx2)
  } # }
```
