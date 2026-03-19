# taxRevenueRotations

calculates taxes of crop rotations as difference between the selected
scenario and the baseline scenario that shall capture the internalized
incentives for crop rotations.

## Usage

``` r
taxRevenueRotations(
  gdx,
  file = NULL,
  level = "regglo",
  penalty = "onlyTaxRevenue"
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  aggregation level, reg, glo or regglo

- penalty:

  "OnlyTaxRevenue" provides the tax Revenues from a rotation
  tax/subsidy. "OnlyInternalizedServices" provides the penalty by
  foregone Ecosystem Services, the part of the externality which is
  internalized by the farmer independent of the tax. "FullPenalty"
  provides the sum of both, which is what the model sees.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- wageRent(gdx)
} # }
```
