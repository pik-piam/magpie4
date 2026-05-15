# CostCapital

Reads data to calculate capital stocks

## Usage

``` r
CostCapital(gdx, type = "stocks", file = NULL, level = "cell")
```

## Arguments

- gdx:

  GDX file

- type:

  either capital stocks ("stocks") or overall capital investment
  "investment"

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

## Value

A MAgPIE object containing values related with overall value of
production \[million US\$17\]

## Author

Edna Molina Bacca

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- CostCapital(gdx)
  } # }
```
