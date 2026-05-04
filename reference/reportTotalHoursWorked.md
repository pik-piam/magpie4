# reportTotalHoursWorked

reports total hours worked in crop+livestock production (and maccs) from
MAgPIE results

## Usage

``` r
reportTotalHoursWorked(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  spatial aggregation: "reg", "glo", "regglo"

## Value

total hours worked as MAgPIE object

## Total hours worked variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Labor\|Total Hours Worked | mio h | Total hours worked in crop and livestock production |

## Author

Debbora Leip

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportTotalHoursWorked(gdx)
  } # }

```
