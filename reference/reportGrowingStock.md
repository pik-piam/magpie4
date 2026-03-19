# reportGrowingStock

reports Growing stocks for woody materials

## Usage

``` r
reportGrowingStock(
  gdx,
  indicator = "relative",
  detail = FALSE,
  level = "regglo"
)
```

## Arguments

- gdx:

  GDX file

- indicator:

  If the reported numbers are relative (mio m3/ha) or absolute (mio.
  m3). Default is relative.

- detail:

  if detail=FALSE, the subcategories of groups are not reported.

- level:

  aggregation level of returned data ("regglo" by default)

## Value

production as MAgPIE object. Unit: see names

## Growing stock variables

|                                                 |       |                                       |
|-------------------------------------------------|-------|---------------------------------------|
| Name                                            | Unit  | Meta                                  |
| Resources\|Growing Stock\|relative\|Forest      | m3/ha | Relative growing stock in forests     |
| Resources\|Growing Stock\|relative\|Plantations | m3/ha | Relative growing stock in plantations |
| Resources\|Growing Stock\|absolute\|Forest      | Mm3   | Absolute growing stock in forests     |
| Resources\|Growing Stock\|absolute\|Plantations | Mm3   | Absolute growing stock in plantations |

## Author

Abhijeet Mishra

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportGrowingStock(gdx)
  } # }
```
