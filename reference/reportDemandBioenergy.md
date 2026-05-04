# reportDemandBioenergy

reports Bioenergy Demand in EJ/yr

## Usage

``` r
reportDemandBioenergy(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

Bioenergy demand as MAgPIE object (EJ/yr)

## Bioenergy demand variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Demand\|Bioenergy | EJ/yr | Total bioenergy demand |
| Demand\|Bioenergy\|++\|2nd generation | EJ/yr | Second generation bioenergy demand (dedicated crops and residues) |
| Demand\|Bioenergy\|++\|1st generation | EJ/yr | First generation bioenergy demand (oils, ethanol) |
| Demand\|Bioenergy\|++\|Traditional Burning | EJ/yr | Traditional biomass burning demand |

## Author

Florian Humpenoeder, Kristine Karstens

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportDemandBioenergy()
  } # }
```
