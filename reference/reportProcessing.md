# reportProcessing

reportes processing input and output quantities primary-to-process or
primary-to-secondary

## Usage

``` r
reportProcessing(
  gdx,
  detail = TRUE,
  indicator = "primary_to_process",
  level = "regglo"
)
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- indicator:

  "primary_to_process" for process or "secondary_from_primary" for
  secondary product output

- level:

  aggregation level of returned data ("regglo" by default)

## Value

processing demand as MAgPIE object (Mt DM)

## Processing demand variables

|                                                             |          |                                                              |
|-------------------------------------------------------------|----------|--------------------------------------------------------------|
| Name                                                        | Unit     | Meta                                                         |
| Demand\|Processing\|++\|Crops                               | Mt DM/yr | Primary crop products processed into secondary products      |
| Demand\|Processing\|++\|Livestock products                  | Mt DM/yr | Primary livestock products processed into secondary products |
| Processing\|Raw material\|Processed into Secondary products | Mt DM/yr | Total raw materials processed into secondary products        |

## Author

David Chen, Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportProcessing(gdx = gdx, detail = TRUE, indicator = "primary_to_process")
} # }

```
