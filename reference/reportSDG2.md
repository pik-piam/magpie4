# reportSDG2

reports all SDG indicators relevant for SD2 - Hunger

## Usage

``` r
reportSDG2(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

MAgPIE object

## SDG2 Hunger variables

|                                                            |                     |                                                        |
|------------------------------------------------------------|---------------------|--------------------------------------------------------|
| Name                                                       | Unit                | Meta                                                   |
| SDG\|SDG02\|Prevalence of underweight                      | million             | Population with underweight BMI                        |
| SDG\|SDG02\|Prevalence of underweight\|Children            | million             | Children under 5 with underweight BMI                  |
| SDG\|SDG02\|Food availability                              | kcal/cap/day        | Daily per-capita caloric availability                  |
| SDG\|SDG02\|Food expenditure share                         | income              | Share of income spent on food (value added)            |
| SDG\|SDG02\|Agricultural primary product expenditure share | income              | Share of income spent on agricultural primary products |
| SDG\|SDG02\|Agricultural commodity price index wrt 2020    | 1                   | Food price index relative to 2020 baseline             |
| SDG\|SDG02\|Prevalence of obesity\|Children                | million             | Children under 5 with obesity                          |
| SDG\|SDG02\|Investment in AgR&D                            | million US\$2017/yr | Investment in agricultural research and development    |

## Author

Benjamin Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportSDG2(gdx)
  } # }

```
