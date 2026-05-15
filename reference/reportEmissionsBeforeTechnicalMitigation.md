# reportEmissionsBeforeTechnicalMitigation

reports GHG emissions before technical mitigation. Technical abatement
includes all abatement done in the MACC curves, but exclude endogenous
mitigation. These emissions are NOT the standard reporting emissions,
but used for special purposes like remind-magpie coupling.

## Usage

``` r
reportEmissionsBeforeTechnicalMitigation(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

MAgPIE object (Unit: Mt CO2/yr, Mt N2O/yr and Mt CH4/yr)

## Emissions before technical mitigation variables

|                                                                                           |           |                                                 |
|-------------------------------------------------------------------------------------------|-----------|-------------------------------------------------|
| Name                                                                                      | Unit      | Meta                                            |
| Emissions before technical mitigation\|N2O\|Land\|+\|Agriculture                          | Mt N2O/yr | N2O emissions before MACC abatement             |
| Emissions before technical mitigation\|N2O\|Land\|Agriculture\|+\|Animal Waste Management | Mt N2O/yr | N2O from animal waste before mitigation         |
| Emissions before technical mitigation\|N2O\|Land\|Agriculture\|+\|Agricultural Soils      | Mt N2O/yr | N2O from agricultural soils before mitigation   |
| Emissions before technical mitigation\|CH4\|Land\|+\|Agriculture                          | Mt CH4/yr | CH4 emissions before MACC abatement             |
| Emissions before technical mitigation\|CH4\|Land\|Agriculture\|+\|Enteric fermentation    | Mt CH4/yr | CH4 from enteric fermentation before mitigation |

## Author

Florian Humpenoeder, Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportEmissionsBeforeTechnicalMitigation(gdx)
  } # }
```
