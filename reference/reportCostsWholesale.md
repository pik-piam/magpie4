# reportCostsWholesale

Reads data to calculate wholesale costs

## Usage

``` r
reportCostsWholesale(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in superAggregate

## Value

A MAgPIE object containing values related with costs wholesale trade
(million US\$2017/yr)

## Wholesale cost variables

|                        |                     |                             |
|------------------------|---------------------|-----------------------------|
| Name                   | Unit                | Meta                        |
| Costs\|Wholesale Costs | million US\$2017/yr | Total wholesale trade costs |

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportCostsWholesale(gdx)
} # }
```
