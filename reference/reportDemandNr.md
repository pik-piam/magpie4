# reportDemandNr

Similar to reportDemand, but for nitrogen. reports Demand for Food,
Feed, Processing, Material, Bioenergy, Seed and Supply Chain Loss

## Usage

``` r
reportDemandNr(gdx, detail = FALSE)
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

## Value

demand as MAgPIE object (Mt DM)

## Nitrogen demand variables

|                    |          |                                |
|--------------------|----------|--------------------------------|
| Name               | Unit     | Meta                           |
| Demand\|Food       | Mt Nr/yr | Nitrogen demand for food       |
| Demand\|Feed       | Mt Nr/yr | Nitrogen demand for feed       |
| Demand\|Processing | Mt Nr/yr | Nitrogen demand for processing |
| Demand\|Bioenergy  | Mt Nr/yr | Nitrogen demand for bioenergy  |
| Demand\|Material   | Mt Nr/yr | Nitrogen demand for materials  |

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportDemand()
  } # }

```
