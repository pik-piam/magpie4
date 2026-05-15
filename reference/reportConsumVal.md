# reportConsumVal

reports MAgPIE consumption value

## Usage

``` r
reportConsumVal(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Magpie object associated with the consumption value

## Consumption value variables

|                          |                     |                                         |
|--------------------------|---------------------|-----------------------------------------|
| Name                     | Unit                | Meta                                    |
| Value\|Consumption Value | million US\$2017/yr | Total value of agricultural consumption |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportConsumVal(gdx)
} # }
```
