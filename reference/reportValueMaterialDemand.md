# reportValueMaterialDemand

reports value of material demand

## Usage

``` r
reportValueMaterialDemand(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

magpie object

## Material demand value variables

|                                       |                     |                                            |
|---------------------------------------|---------------------|--------------------------------------------|
| Name                                  | Unit                | Meta                                       |
| Value\|Bioeconomy Demand              | million US\$2017/yr | Total value of bioeconomy material demand  |
| Value\|Bioeconomy Demand\|+\|Crops    | million US\$2017/yr | Value of crop products for material demand |
| Value\|Bioeconomy Demand\|+\|Residues | million US\$2017/yr | Value of residues for material demand      |

## Author

David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportValueMaterialDemand(gdx)
} # }

```
