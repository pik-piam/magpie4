# CostTC

Reads data on TC costs

## Usage

``` r
CostTC(gdx, file = NULL, level = "reg")
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

A MAgPIE object containing values related with overall value of
production \[million US\$17\]

## Author

David Chen

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- CostTC(gdx)
  } # }
```
