# reportPriceWoodyBiomass

reports woody biomass prices (land rent)

## Usage

``` r
reportPriceWoodyBiomass(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

land prices as MAgPIE object Unit: see names

## Woody biomass price variables

|                  |              |                |
|------------------|--------------|----------------|
| Name             | Unit         | Meta           |
| Prices\|Wood     | US\$2017/tDM | Wood price     |
| Prices\|Woodfuel | US\$2017/tDM | Woodfuel price |

## Author

Abhijeet Mishra

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportPriceWoodyBiomass(gdx)
  } # }

```
