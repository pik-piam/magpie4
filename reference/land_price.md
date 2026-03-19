# land_price

Calculates MAgPIE MAgPIE land shadow prices based on a gdx file

## Usage

``` r
land_price(
  gdx,
  file = NULL,
  level = "reg",
  ignore_lowbound = FALSE,
  absolute = TRUE,
  digits = 4
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

- ignore_lowbound:

  Some shadow prices are positive (see Details), corresponding to a
  lower bound for that pool. `TRUE` sets them to 0. Default value:
  `FALSE`.

- absolute:

  Should the absolute value of all the marginals be taken into
  calculations? `TRUE` (default) of `FALSE`. See Details.

- digits:

  rounding accuracy for the output

## Value

A MAgPIE object containing the land shadow prices (US\$2017/ha).

## Details

The land price is obtained through marginals of the "oq_cropland"
constraint. The majority of these marginals are negative values, and a
negligible number of them are positive. This is the consequence of the
constraint binding either on upper or lower level. The parameter
`ignore_lowbound` removes all the positive marginals from land price
calculation (negligible), and parameter `absolute` transforms them into
negative values (to be all together reported as positive values at the
final calculation).

## Author

Markus Bonsch, Misko Stevanovic

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- land_price(level="regglo", products="kcr")
  } # }
```
