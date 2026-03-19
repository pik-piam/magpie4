# primaryPerSecondary

Calculates the amount of primary products needed per unit of secondary
(and tertiary) product, accounting for processing chains and co-product
allocation. This function traces back from secondary/tertiary products
through processing stages to the original primary crop inputs.

## Usage

``` r
primaryPerSecondary(gdx, file = NULL, level = "reg", allocation = "mass")
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in gdxAggregate

- allocation:

  Method for allocating primary product use when co-products are
  produced. Options: "mass" (default): Allocation based on dry matter
  content of outputs "value": Allocation based on economic value of
  outputs "none": No allocation - full primary input attributed to each
  output (sum of allocations \> 100 percent when co-products exist)

## Value

MAgPIE object containing the amount of primary product needed per unit
of secondary product (tDM/tDM)

## See also

[`production`](production.md), [`trade`](trade.md)

## Author

Kristine Karstens, David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- primaryPerSecondary(gdx)
} # }
```
