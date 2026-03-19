# YieldsCropCalib

Reads potential yields after calibration

## Usage

``` r
YieldsCropCalib(gdx, file = NULL, level = "cell", tau = FALSE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation

- tau:

  if TRUE add effect of TAU to the yield

## Value

A MAgPIE object containing values of potential yields after the
calibration routines

## Author

Edna Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- YieldsCropCalib(gdx)
} # }
```
