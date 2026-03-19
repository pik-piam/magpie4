# CostTransport

reads costs entering the objective function from a MAgPIE gdx file

## Usage

``` r
CostTransport(gdx, file = NULL, level = "cell", sum = FALSE)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate.

- sum:

  total costs (TRUE) or detailed costs (FALSE)

## Value

A MAgPIE object containing the transport costs \[million US\$17\]

## Author

David Chen

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- CostTransport(gdx)
  } # }
```
