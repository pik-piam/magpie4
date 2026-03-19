# reportSDG3

reports all SDG indicators relevant for SDG3 - Health

## Usage

``` r
reportSDG3(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

MAgPIE object

## SDG3 Health variables

|                                                |              |                                              |
|------------------------------------------------|--------------|----------------------------------------------|
| Name                                           | Unit         | Meta                                         |
| SDG\|SDG03\|Prevalence of overweight           | million      | Population with overweight BMI               |
| SDG\|SDG03\|Prevalence of obesity              | million      | Population with obesity                      |
| SDG\|SDG03\|Prevalence of overweight\|Children | million      | Children under 5 with overweight BMI         |
| SDG\|SDG03\|Prevalence of obesity\|Children    | million      | Children under 5 with obesity                |
| SDG\|SDG03\|Consumption of alcohol             | kcal/cap/day | Daily per-capita caloric intake from alcohol |

## Author

Benjamin Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSDG3(gdx)
  } # }

```
