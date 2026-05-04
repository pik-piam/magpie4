# FoodExpenditureShare

Calculates the livestock share from the food demand model

## Usage

``` r
FoodExpenditureShare(
  gdx,
  level = "reg",
  after_shock = TRUE,
  products = "kfo",
  product_aggr = TRUE,
  valueAdded = FALSE
)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation. can be "iso","reg","regglo","glo"

- after_shock:

  FALSE is using the exogenous real income and the prices before a
  shock, TRUE is using the endogeenous real income that takes into
  account food price change on real income

- products:

  selected products or sets of products

- product_aggr:

  if true, aggregation over products

- valueAdded:

  TRUE to include post farmgate value added in the expenditure share

## Value

magpie object with per capita consumption

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- FoodExpenditureShare(gdx)
  } # }
```
