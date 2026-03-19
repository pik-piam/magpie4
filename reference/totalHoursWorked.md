# totalHoursWorked

returns total hours worked per year in crop+livestock production from
MAgPIE results

## Usage

``` r
totalHoursWorked(gdx, level = "reg", file = NULL)
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation to report employment ("reg", "glo", or "regglo")

- file:

  a file name the output should be written to using write.magpie

## Value

total hours worked in agriculture per year

## Author

Debbora Leip

## Examples

``` r
if (FALSE) { # \dontrun{
x <- totalHoursWorked(gdx)
} # }
```
