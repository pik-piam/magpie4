# reportManure

Reports the Nitrogen in Manure of all animals for future MAgPIE
projections

## Usage

``` r
reportManure(gdx, nutrient = "nr", level = "regglo")
```

## Arguments

- gdx:

  GDX file

- nutrient:

  nr, p, c...

- level:

  aggregation level of returned data ("regglo" by default)

## Manure variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Resources\|Nitrogen\|Manure | Mt Nr/yr | Total manure nitrogen production |
| Resources\|Nitrogen\|Manure\|++\|Confinement | Mt Nr/yr | Manure from confined livestock |
| Resources\|Nitrogen\|Manure\|++\|Grazing | Mt Nr/yr | Manure deposited during grazing |
| Resources\|Nitrogen\|Manure\|+\|Ruminants | Mt Nr/yr | Manure from ruminant animals |
| Resources\|Nitrogen\|Manure\|+\|Monogastric | Mt Nr/yr | Manure from monogastric animals |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportManure(gdx)
  } # }
```
