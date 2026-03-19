# CostsWithoutIncentives

calculates agricultural costs without taxes, incentives and technical
penalty costs (i.e. GHG taxes and BII incentives)

## Usage

``` r
CostsWithoutIncentives(gdx, file = NULL, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  aggregation level, reg, glo or regglo

## Value

A MAgPIE object containing the costs without taxes, incentives and
technical penalty costs \[million US\$17\]

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
  x <- CostsWithoutIncentives(gdx)
} # }
```
