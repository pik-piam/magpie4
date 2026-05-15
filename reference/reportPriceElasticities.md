# reportHunger

Calculates the share of people living in hunger.

## Usage

``` r
reportPriceElasticities(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

magpie object with hunger (mio people) or hunger share

## Price elasticity variables

|                                                             |      |                                                  |
|-------------------------------------------------------------|------|--------------------------------------------------|
| Name                                                        | Unit | Meta                                             |
| Food Supply\|PriceElasticities\|Total Calories              | %/%  | Price elasticity of total calorie demand         |
| Food Supply\|PriceElasticities\|Staples                     | %/%  | Price elasticity of staple food demand           |
| Food Supply\|PriceElasticities\|Livestock Products          | %/%  | Price elasticity of livestock product demand     |
| Food Supply\|PriceElasticities\|Vegetables, Fruits and Nuts | %/%  | Price elasticity of fruits and vegetables demand |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportHunger(gdx)
  } # }

```
