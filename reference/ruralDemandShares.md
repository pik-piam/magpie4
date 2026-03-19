# ruralDemandShares

reports rural demand shares based on local consumption

## Usage

``` r
ruralDemandShares(
  gdx,
  type = "tradOnly",
  level = "reg",
  product_aggr = TRUE,
  file = NULL
)
```

## Arguments

- gdx:

  GDX file

- type:

  Type of ratio that should be calculated

  - `all`: How much rural & trad demand as a share of all demand is
    satisfied locally

  - `tradOnly`: How much rural & trad demand as a share of rural & trad
    demand is satisfied locally

  - `potential`: How much total gridded demand is potentially satisfied
    by gridded production

- level:

  spatial aggregation to report employment ("reg", "glo" or "regglo")

- product_aggr:

  sum over products if TRUE

- file:

  a file name the output should be written to using write.magpie

## Value

share of food consumed locally

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- localDemandShares(gdx)
} # }
```
