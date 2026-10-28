# reportFireEmissions.R

reads land cover data and deforestation data to produce future estimates
of fire emissions based on extrapolating GFED4.1s data from 2003-2016.
Emissions are area x rho x emission factor, where rho
(inst/extdata/GFED_regional_dm_rho_scaled.csv) is the regional GFED
dry-matter mass of a fire type per m2 of the historical area of the
class it is applied to; see the provenance file beside it. The run's
areas: forest (primforest + secdforest + forestry) for boreal,
temperate, small deforestation and peat fires; past + other for savanna
and grassland fires; crop for agricultural fires (not reported); for
large deforestation fires, the natural-forest reduction without timber
harvest (primary and secondary forest reduction minus their harvested
area, which regrows) plus the secondary-forest disturbance loss
(shifting agriculture), per year. Fire CO2 is gross: deforestation fire
CO2 overlaps with MAgPIE's land-use change CO2, and savanna and forest
fire CO2 is largely balanced by regrowth. Peat fire CO2 does not overlap
(MAgPIE's peat CO2 uses the IPCC Wetlands Supplement factors for drained
soils, which exclude fire). Do not add fire CO2 to land-use change CO2.

## Usage

``` r
reportFireEmissions(gdx, level = "reg")
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "reg" (regional), "glo" (global),
  "regglo" (regional and global)

## Value

emissions as MAgPIE object (unit: Mt X/yr, plus cumulative Mt X/yr)

## Fire emission variables

Reported for X = BC, CO, CO2, NO2, OC, SO2, N2O, CH4, NH3 and VOC. NO2
is NOx reported as mass of NO2 (GFED gives NOx as mass of NO; converted
by 46/30).

|                                                                       |         |                              |
|-----------------------------------------------------------------------|---------|------------------------------|
| Name                                                                  | Unit    | Meta                         |
| Emissions\|X\|AFOLU\|Land\|Fires                                      | Mt X/yr | Total X emissions from fires |
| Emissions\|X\|AFOLU\|Land\|Fires\|+\|Forest Burning                   | Mt X/yr | X from forest fires          |
| Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Boreal Forest    | Mt X/yr | X from boreal                |
| forest fires                                                          |         |                              |
| Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Temperate Forest | Mt X/yr | X from temperate             |
| forest fires                                                          |         |                              |
| Emissions\|X\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Tropical Forest  | Mt X/yr | X from tropical              |
| deforestation fires                                                   |         |                              |
| Emissions\|X\|AFOLU\|Land\|Fires\|+\|Grassland Burning                | Mt X/yr | X from grassland/savanna     |
| fires                                                                 |         |                              |
| Emissions\|X\|AFOLU\|Land\|Fires\|+\|Peat Burning                     | Mt X/yr | X from peat fires            |

## Author

Michael Crawford

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportFireEmissions(gdx, level = "glo")
  } # }
```
