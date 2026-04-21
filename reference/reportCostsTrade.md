# reportCostsTrade

Reports MAgPIE bilateral trade cost components

## Usage

``` r
reportCostsTrade(gdx, level = "regglo", sum = FALSE)
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

- sum:

  whether to sum across subcategories

## Value

A MAgPIE object containing values related to trade costs (million
US\$2017/yr)

## Author

David M Chen
