# reportValueTrade

reports trade value

## Usage

``` r
reportValueTrade(gdx, detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if true, provides estimates for all commodities, otherwhise aggregates
  some groups

- level:

  aggregation level of returned data ("regglo" by default)

## Value

trade value as MAgPIE object Unit: see names

## Trade value variables

|                          |                     |                                         |
|--------------------------|---------------------|-----------------------------------------|
| Name                     | Unit                | Meta                                    |
| Trade Value\|Net-Exports | million US\$2017/yr | Net trade value (exports minus imports) |
| Trade Value\|Exports     | million US\$2017/yr | Gross export value                      |
| Trade Value\|Imports     | million US\$2017/yr | Gross import value                      |

## Author

Florian Humpenoeder

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportValueTrade(gdx)
  } # }

```
