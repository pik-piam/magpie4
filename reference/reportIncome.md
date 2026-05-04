# reportIncome

reports income

## Usage

``` r
reportIncome(gdx, type = "ppp", level = "regglo")
```

## Arguments

- gdx:

  GDX file

- type:

  ppp for purchase power parity, mer for market exchange rate

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

Annual per capita and total income as MAgPIE object (US\$2017 MER/cap/yr
and million US\$17 PPP/yr)

## Income variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Income per capita PPP | US\$2017 PPP/cap/yr | GDP per capita converted to US\$2017 using purchasing power parity (PPP) |
| Income PPP | million US\$2017 PPP/yr | GDP converted to US\$2017 using purchasing power parity (PPP) |
| Income per capita MER | US\$2017 MER/cap/yr | GDP per capita converted to US\$2017 at market exchange rate (MER) |
| Income MER | million US\$2017 MER/yr | GDP converted to US\$2017 at market exchange rate (MER) |

## Author

Florian Humpenoeder, Isabelle Weindl, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
x <- reportIncome(gdx)
} # }
```
