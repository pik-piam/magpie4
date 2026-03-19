# discountRates

reads discount rates from a MAgPIE gdx file

## Usage

``` r
discountRates(gdx, file = NULL, level = "reg")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

## Value

A MAgPIE object containing discount rates used in the model

## Author

Xiaoxi Wang

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- discountRates(gdx)
  } # }
```
