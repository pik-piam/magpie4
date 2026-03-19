# expenditureIndexFood

calculates food expenditure index (baseyear = 100) corrected for ghg
emission costs based on a MAgPIE gdx file

## Usage

``` r
expenditureIndexFood(
  gdx,
  file = NULL,
  level = "reg",
  products = "kfo",
  basketyear = "y2010",
  baseyear = "y2010",
  round = TRUE,
  ghgtax = TRUE,
  valueAdded = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  File the output should be written to using write.magpie

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global) or any other aggregation level defined
  in mapping

- products:

  Selection of products (either by naming products, e.g. "tece", or
  naming a set, e.g."kcr")

- basketyear:

  Year of reference food basket (should be in the past for comparison of
  different runs to have identical and comparable food basket)

- baseyear:

  Baseyear of the price index

- round:

  Rounded result (TRUE or FALSE)

- ghgtax:

  Correction of food price expenditure for ghg emission costs (TRUE or
  FALSE)

- valueAdded:

  whether to include value added

## Value

A MAgPIE object containing food price expenditure index, in 2017Int\$PPP

## Author

Felicitas Beier, David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- expenditureIndexFood(gdx)
} # }
```
