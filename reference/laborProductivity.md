# laborProductivity

calculates labor productivity in crop sector (kg DM per hour) from a
MAgPIE gdx file

## Usage

``` r
laborProductivity(gdx, level = "reg", productAggr = TRUE)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation to report productivity ("cell","reg", "regglo",
  "glo")

- productAggr:

  Aggregate over products or not (boolean)

## Value

labor productivity in crop sector (kg DM per hour)

## Author

Xiaoxi Wang, Ruiying Du, Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- laborProductivity(gdx)
} # }
```
