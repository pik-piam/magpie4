# priceIndex

calcluates price indicies based on a MAgPIE gdx file

## Usage

``` r
priceIndex(
  gdx,
  file = NULL,
  level = "reg",
  products = "kall",
  index = "lasp",
  chain = FALSE,
  baseyear = "y2005",
  round = TRUE,
  type = "consumer",
  product_aggr = TRUE
)
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

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set,e.g."kcr")

- index:

  "lasp" (Laspeyres-Index: baseyear weighting), "paas" (Paasche-Index:
  current weighting), "fish" (Fisher-Index: geometric mean of "lasp" and
  "paas")

- chain:

  Chain Index: if true, the base period for each time period is the
  immediately preceding time period. Can be combined with all of the
  above indices

- baseyear:

  baseyear of the price index

- round:

  shall the results be rounded?

- type:

  For whom are the prices important? "producer" are the prices that
  farmer face, as they also produce intermediate products (seed, feed).
  "consumer" are the prices for the end consumer faces (supermarket,
  bioenergy plant). Currently, the only difference is the basket
  composition (ideally, also prices should differ between regions)

- product_aggr:

  aggregate over products or not (boolean)

## Value

A MAgPIE object containing price indices for consumers or producers
(depending on type)

## Author

Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- priceIndex(gdx)
  } # }
```
