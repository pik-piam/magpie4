# reportWaterIndicators

reports a set of water indicators

## Usage

``` r
reportWaterIndicators(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

MAgPIE object

## Water indicator variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Water\|Environmental flow violation volume | km3/yr | Volume of environmental flow violations |
| Water\|Environmental flow violation share of total water withdrawals | share | EFV share of human water withdrawals |
| Water\|Environmental flow violation share of water availability | share | EFV share of water availability |
| Water\|Irrigated Area suffering under Environmental Flow Violation | Mha | Area in clusters with EFV |
| Water\|Share of total Irrigated Area suffering from Environmental Flow Violations | share | Share of irrigated area in EFV clusters |
| Water\|Withdrawal to Availability ratio | fraction | Water stress indicator |

## Author

Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportWaterIndicators(gdx)
} # }
```
