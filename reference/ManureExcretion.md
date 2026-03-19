# ManureExcretion

downscales Manure Excretion

## Usage

``` r
ManureExcretion(
  gdx,
  level = "reg",
  products = "kli",
  awms = c("grazing", "stubble_grazing", "fuel", "confinement"),
  agg = TRUE
)
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level: glo, reg, cell, grid, iso

- products:

  livestock products

- awms:

  large animal waste management categories:
  "grazing","stubble_grazing","fuel","confinement"),

- agg:

  aggregation over "awms" or over "products".

## Value

MAgPIE object

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- ManureExcretion(gdx)
  } # }
```
