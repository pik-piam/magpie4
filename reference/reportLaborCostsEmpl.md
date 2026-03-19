# reportLaborCostsEmpl

reports MAgPIE labor costs that go into employment calculation

## Usage

``` r
reportLaborCostsEmpl(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

magpie object with labor costs

## Labor costs for employment variables

|                                                     |                     |                                                   |
|-----------------------------------------------------|---------------------|---------------------------------------------------|
| Name                                                | Unit                | Meta                                              |
| Labor\|Employment\|Labor costs linked to employment | million US\$2017/yr | Total labor costs used for employment calculation |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportLaborCostsEmpl(gdx)
  } # }
```
