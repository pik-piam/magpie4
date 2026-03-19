# factorCostShares

returns labor and capital cost share out of factor costs (i.e. labor +
capital)

## Usage

``` r
factorCostShares(
  gdx,
  type = "optimization",
  products = "kcr",
  level = "reg",
  file = NULL
)
```

## Arguments

- gdx:

  GDX file

- type:

  - "requirements": shares from factor requirements

  - "optimization": cost shares between labor and capital costs in
    optimization

  - "accounting": cost shares based on accounting of labor and capital
    costs

- products:

  products for which cost shares should be reported, kcr or kli

- level:

  spatial aggregation to report employment ("reg", "glo" or "regglo")

- file:

  a file name the output should be written to using write.magpie

## Value

labor and capital cost share out of factor costs

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- factorCostShares(gdx)
} # }
```
