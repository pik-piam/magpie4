# reportSOM

Report soil organic carbon stock size for future MAgPIE projections

## Usage

``` r
reportSOM(gdx, baseyear = 1995, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- baseyear:

  baseyear for calculating carbon stock change

- level:

  aggregation level of returned data ("regglo" by default)

## Actual soil carbon stock variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm | Mt C | Total soil organic carbon stock in top 30 cm |
| Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | SOC stock in cropland soils |
| Resources\|Soil Carbon\|Actual\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | SOC stock in non-cropland soils |

## Soil carbon stock change variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm | Mt C wrt baseyear | Change in total SOC stock relative to base year |
| Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm\|+\|Cropland Soils | Mt C wrt baseyear | Change in cropland SOC stock relative to base year |
| Resources\|Soil Carbon\|Actual\|Stock Change\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C wrt baseyear | Change in non-cropland SOC stock relative to base year |

## Actual soil carbon density variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm | tC/ha | Average SOC density in top 30 cm |
| Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Cropland Soils | tC/ha | SOC density in cropland soils |
| Resources\|Soil Carbon\|Actual\|Density\|SOC in top 30 cm\|Noncropland Soils | tC/ha | SOC density in non-cropland soils |

## Target soil carbon stock variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm | Mt C | Target total SOC stock at steady state |
| Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm\|+\|Cropland Soils | Mt C | Target SOC stock in cropland soils |
| Resources\|Soil Carbon\|Target\|Stock\|SOC in top 30 cm\|+\|Noncropland Soils | Mt C | Target SOC stock in non-cropland soils |

## Target soil carbon density variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm | tC/ha | Target average SOC density at steady state |
| Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm\|Cropland Soils | tC/ha | Target SOC density in cropland soils |
| Resources\|Soil Carbon\|Target\|Density\|SOC in top 30 cm\|Noncropland Soils | tC/ha | Target SOC density in non-cropland soils |

## Actual carbon share variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm | tC/tC | SOC relative to natural state |
| Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm\|Cropland Soils | tC/tC | Cropland SOC relative to natural state |
| Resources\|Soil Carbon\|Actual\|Carbon Share\|SOC in top 30 cm\|Noncropland Soils | tC/tC | Non-cropland SOC relative to natural state |

## Target carbon share variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm | tC/tC | Target SOC share at steady state |
| Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm\|Cropland Soils | tC/tC | Target cropland SOC share at steady state |
| Resources\|Soil Carbon\|Target\|Carbon Share\|SOC in top 30 cm\|Noncropland Soils | tC/tC | Target non-cropland SOC share at steady state |

## Author

Kristine Karstens

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSOM(gdx)
  } # }
```
