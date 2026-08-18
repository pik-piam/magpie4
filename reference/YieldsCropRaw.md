# YieldsCropRaw

Reads uncalibrated potential yields, i.e. before the calibration
routines

## Usage

``` r
YieldsCropRaw(gdx, file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation

## Value

A MAgPIE object containing values of potential yields as they come from
the crop model, before the calibration routines are applied.

## Author

Edna Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- YieldsCropRaw(gdx)
} # }
```
