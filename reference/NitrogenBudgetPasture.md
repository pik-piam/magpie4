# NitrogenBudgetPasture

calculates projections of Nitrogen Budgets for Croplands from a MAgPIE
gdx file

## Usage

``` r
NitrogenBudgetPasture(gdx, include_emissions = FALSE, level = "reg")
```

## Arguments

- gdx:

  GDX file

- include_emissions:

  TRUE also divides the N surplus into different emissions

- level:

  aggregation level, reg, glo or regglo, cell, grid, iso

## Author

Benjamin Leon Bodirsky, Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- NitrogenBudgetPasture(gdx)
} # }
```
