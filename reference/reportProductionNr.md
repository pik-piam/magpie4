# reportProductionNr

reports production in Nr analogous to reportProduction

## Usage

``` r
reportProductionNr(gdx, detail = FALSE)
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail = FALSE, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

## Value

production as MAgPIE object. Unit: see names

## Nitrogen production variables

|                                      |          |                                           |
|--------------------------------------|----------|-------------------------------------------|
| Name                                 | Unit     | Meta                                      |
| Production Nr                        | Mt Nr/yr | Total nitrogen in agricultural production |
| Production Nr\|+\|Crop products      | Mt Nr/yr | Nitrogen in crop production               |
| Production Nr\|+\|Livestock products | Mt Nr/yr | Nitrogen in livestock production          |

## Author

Benjamin Leon Bodirsky

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportProductionNr(gdx)
  } # }

```
