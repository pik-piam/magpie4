# NitrogenBudgetWithdrawals

calculates projections of Nitrogen Budgets withdrawals for Croplands
from a MAgPIE gdx file

## Usage

``` r
NitrogenBudgetWithdrawals(gdx, kcr = "sum", net = TRUE, level = "reg")
```

## Arguments

- gdx:

  GDX file

- kcr:

  "sum" provides the totals over all crops, "kcr" provides outputs by
  kcr

- net:

  TRUE only provides total net-withdrawals, otherwise all categories are
  returned (fixation and seed are returned positive, not negative)

- level:

  aggregation level, reg, glo or regglo, cell, grid or iso

## Author

Benjamin Leon Bodirsky, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- NitrogenBudgetWithdrawals(gdx)
} # }
```
