# agEmployment

returns employment in crop+livestock production from MAgPIE results

## Usage

``` r
agEmployment(gdx, type = "absolute", detail = TRUE, level = "reg", file = NULL)
```

## Arguments

- gdx:

  GDX file

- type:

  "absolute" for total number of people employed, "share" for share out
  of working age population

- detail:

  if TRUE, employment is disaggregated to crop products, livestock
  products and (if available) mitigation measures, if FALSE only
  aggregated employment is reported

- level:

  spatial aggregation to report employment ("iso", "reg", "glo" or
  "regglo", if type is "absolute" also "grid")

- file:

  a file name the output should be written to using write.magpie

## Value

employment in agriculture as absolute value or as percentage of working
age population

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- agEmployment(gdx)
} # }
```
