# reportAgEmployment

reports employment in crop+livestock production from MAgPIE results

## Usage

``` r
reportAgEmployment(gdx, type = "absolute", detail = FALSE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- type:

  "absolute" for total number of people employed, "share" for share out
  of working age population

- detail:

  if TRUE, employment is disaggregated to crop and livestock production,
  if FALSE only aggregated employment is reported

- level:

  spatial aggregation: "reg", "glo", "regglo", "iso"

## Value

agricultural employment as MAgPIE object

## Agricultural employment variables

|                                                                            |            |                                                            |
|----------------------------------------------------------------------------|------------|------------------------------------------------------------|
| Name                                                                       | Unit       | Meta                                                       |
| Labor\|Employment\|Agricultural employment                                 | mio people | Total agricultural employment                              |
| Labor\|Employment\|Agricultural employment\|+\|Crop products               | mio people | Employment in crop production                              |
| Labor\|Employment\|Agricultural employment\|+\|Livestock products          | mio people | Employment in livestock production                         |
| Labor\|Employment\|Share of working age population employed in agriculture | %          | Agricultural employment as share of working age population |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportAgEmployment(gdx)
  } # }

```
