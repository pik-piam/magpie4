# reportFit

reports fit and error indicators compared to initial values

## Usage

``` r
reportFit(gdx, type = "MAPE", level = "cell")
```

## Arguments

- gdx:

  GDX file

- type:

  type of indicator. Options: R2, MAE, MPE (mean percentage error -
  bias), MAPE (mean absolute percentage error)

- level:

  level at which the regional and global bias should be reported.
  Options "cell" or "grid"

## Value

Selected error indicator

## Cluster-level fit indicator variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Fit\|Cluster\|Land Cover | MAPE/MAE/MPE | Fit indicator for overall land cover |
| Fit\|Cluster\|Land Cover\|Cropland | MAPE/MAE/MPE | Fit indicator for cropland |
| Fit\|Cluster\|Land Cover\|Pastures and Rangelands | MAPE/MAE/MPE | Fit indicator for pasture (when no range distinction) |
| Fit\|Cluster\|Land Cover\|Managed pastures | MAPE/MAE/MPE | Fit indicator for managed pastures (when range available) |
| Fit\|Cluster\|Land Cover\|Rangelands | MAPE/MAE/MPE | Fit indicator for rangelands (when range available) |
| Fit\|Cluster\|Land Cover\|Urban Area | MAPE/MAE/MPE | Fit indicator for urban area |
| Fit\|Cluster\|Land Cover\|Other Land | MAPE/MAE/MPE | Fit indicator for other land |
| Fit\|Cluster\|Land Cover\|Forest\|Natural Forest\|Primary Forest | MAPE/MAE/MPE | Fit indicator for primary forest |
| Fit\|Cluster\|Land Cover\|Forest\|Natural Forest\|Secondary Forest | MAPE/MAE/MPE | Fit indicator for secondary forest |
| Fit\|Cluster\|Land Cover\|Forest\|Planted Forest | MAPE/MAE/MPE | Fit indicator for planted forest |
| Fit\|Cluster\|Land Cover\|Cropland\|{crop} | MAPE/MAE/MPE | Fit indicator for individual crop types |

## Grid-level fit indicator variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Fit\|Grid\|Land Cover | MAPE/MAE/MPE | Grid-level fit indicator for overall land cover |
| Fit\|Grid\|Land Cover\|Cropland | MAPE/MAE/MPE | Grid-level fit indicator for cropland |
| Fit\|Grid\|Land Cover\|Pastures and Rangelands | MAPE/MAE/MPE | Grid-level fit indicator for pasture |
| Fit\|Grid\|Land Cover\|Managed pastures | MAPE/MAE/MPE | Grid-level fit indicator for managed pastures (when range available) |
| Fit\|Grid\|Land Cover\|Rangelands | MAPE/MAE/MPE | Grid-level fit indicator for rangelands (when range available) |
| Fit\|Grid\|Land Cover\|Urban Area | MAPE/MAE/MPE | Grid-level fit indicator for urban area |
| Fit\|Grid\|Land Cover\|Other Land | MAPE/MAE/MPE | Grid-level fit indicator for other land |
| Fit\|Grid\|Land Cover\|Forest\|Natural Forest\|Primary Forest | MAPE/MAE/MPE | Grid-level fit indicator for primary forest |
| Fit\|Grid\|Land Cover\|Forest\|Natural Forest\|Secondary Forest | MAPE/MAE/MPE | Grid-level fit indicator for secondary forest |
| Fit\|Grid\|Land Cover\|Forest\|Planted Forest | MAPE/MAE/MPE | Grid-level fit indicator for planted forest |

## Author

Edna Molina Bacca, Patrick v. Jeetze

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportFit(gdx,type)
  } # }

```
