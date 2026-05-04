# reportFireEmissions.R

reads land cover data and deforestation data to produce future estimates
of fire emissions based on extrapolating GFED data from 2003-2016

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

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Emissions\|CO2\|AFOLU\|Land\|Fires | Mt CO2/yr | Total CO2 emissions from fires |
| Emissions\|CO2\|AFOLU\|Land\|Fires\|+\|Forest Burning | Mt CO2/yr | CO2 from forest fires |
| Emissions\|CO2\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Boreal Forest | Mt CO2/yr | CO2 from boreal |
| forest fires |  |  |
| Emissions\|CO2\|AFOLU\|Land\|Fires\|Forest Burning\|+\|Tropical Forest | Mt CO2/yr | CO2 from tropical |
| deforestation fires |  |  |
| Emissions\|CO2\|AFOLU\|Land\|Fires\|+\|Grassland Burning | Mt CO2/yr | CO2 from grassland/savanna |
| fires |  |  |
| Emissions\|CO2\|AFOLU\|Land\|Fires\|+\|Peat Burning | Mt CO2/yr | CO2 from peat fires |

## Author

Michael Crawford

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportFireEmissions(gdx, level = "glo")
  } # }
```
