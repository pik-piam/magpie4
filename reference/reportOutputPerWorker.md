# reportOutputPerWorker

reports output per worker in crop+livestock production from MAgPIE
results

## Usage

``` r
reportOutputPerWorker(gdx, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- level:

  aggregation level of returned data ("regglo" by default)

## Value

output per worker as MAgPIE object

## Output per worker variables

|                                                 |                 |                                         |
|-------------------------------------------------|-----------------|-----------------------------------------|
| Name                                            | Unit            | Meta                                    |
| Labor\|Productivity\|Monetary output per worker | US\$2017/worker | Monetary output per agricultural worker |

## Author

Debbora Leip

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportOutputPerWorker(gdx)
  } # }

```
