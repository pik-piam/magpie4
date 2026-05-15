# reportAgGDP

reports MAgPIE Agricultural GDP Mio. USD05 MER

## Usage

``` r
reportAgGDP(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

Magpie object

## Agricultural GDP variables

|                        |                     |                                                        |
|------------------------|---------------------|--------------------------------------------------------|
| Name                   | Unit                | Meta                                                   |
| Value\|Agriculture GDP | million US\$2017/yr | Agricultural value added (GDP from agriculture sector) |

## Author

Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportAgGDP(gdx)
} # }
```
