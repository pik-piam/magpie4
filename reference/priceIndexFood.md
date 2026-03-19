# priceIndexFood

calcluates price indicies based on a MAgPIE gdx file

## Usage

``` r
priceIndexFood(
  gdx,
  file = NULL,
  level = "reg",
  index = "lasp",
  chain = FALSE,
  baseyear = "y2005",
  round = TRUE,
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

- index:

  "lasp" (Laspeyres-Index: baseyear weighting), "paas" (Paasche-Index:
  current weighting), "fish" (Fisher-Index: geometric mean of "lasp" and
  "paas")

- chain:

  Chain Index: if true, the base period for each time period is the
  immediately preceding time period. Can be combined with all of the
  above indices

- baseyear:

  baseyear of the price index. type model to take baseyear 2010 with
  literature prices

- round:

  shall the results be rounded?

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
    x <- priceIndexFood(gdx)
  } # }
```
