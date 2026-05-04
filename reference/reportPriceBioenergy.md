# reportPriceBioenergy

reports bioenergy prices

## Usage

``` r
reportPriceBioenergy(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

bioenergy price as MAgPIE object Unit: see names

## Bioenergy price variables

|                   |             |                 |
|-------------------|-------------|-----------------|
| Name              | Unit        | Meta            |
| Prices\|Bioenergy | US\$2017/GJ | Bioenergy price |

## Author

Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportPriceBioenergy(gdx)
  } # }

```
