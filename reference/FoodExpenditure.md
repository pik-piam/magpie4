# FoodExpenditure

Calculates the food expenditure in USD per year

## Usage

``` r
FoodExpenditure(
  gdx,
  level = "reg",
  after_shock = TRUE,
  products = "kfo",
  product_aggr = TRUE,
  per_capita = TRUE,
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
  account food price change on real income, "after_price_before_demand"
  takes into account price changes on real income, but assumes no demand
  adjustment

- products:

  selected products or sets of products

- product_aggr:

  if true, aggregation over products

- per_capita:

  per capita or total population

- valueAdded:

  whether to add the value-added marketing margin to the total
  expenditures

## Value

magpie object with per capita consumption

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- FoodExpenditure(gdx)
  } # }
```
