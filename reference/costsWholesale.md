# costsWholesale

Reads data to calculate wholesale costs

## Usage

``` r
costsWholesale(gdx, file = NULL, level = "regglo")
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

## Value

A MAgPIE object containing values related with costs wholesale trade
\[million US\$17/tDM\]

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- costsWholesale(gdx)
} # }
```
