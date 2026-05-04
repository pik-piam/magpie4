# reportLaborProductivity

reports labor productivity in crop production

## Usage

``` r
reportLaborProductivity(
  gdx,
  productAggr = TRUE,
  type = "physical",
  level = "regglo"
)
```

## Arguments

- gdx:

  GDX file

- productAggr:

  Aggregate over products or not (boolean)

- type:

  type of labor productivity, so far only physical (kg DM / h)

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

labor productivity as MAgPIE object

## Labor productivity variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Labor\|Productivity\|Physical labor productivity\|Crop products | kg DM per hour | Physical labor productivity for crops |

## Author

Debbora Leip

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportLaborProductivity(gdx)
  } # }

```
