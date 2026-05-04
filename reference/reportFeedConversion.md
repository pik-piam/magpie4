# reportFeedConversion

reportes feed demand by animal type

## Usage

``` r
reportFeedConversion(gdx, livestockSystem = TRUE, balanceflow = FALSE)
```

## Arguments

- gdx:

  GDX file

- livestockSystem:

  if TRUE, ruminant products and poultry products are aggregated

- balanceflow:

  If true, feed includes the calibration balanceflow

## Value

feed demand as MAgPIE object (Mt DM)

## Feed conversion variables

|  |  |  |
|----|----|----|
| Name | Unit | Meta |
| Productivity\|Feed conversion | GE per GE | Overall feed conversion efficiency (gross energy) |
| Productivity\|Feed conversion\|Ruminant meat and dairy | GE per GE | Feed conversion for ruminant products |
| Productivity\|Feed conversion\|Poultry meat and eggs | GE per GE | Feed conversion for poultry products |
| Productivity\|Feed conversion\|Pig meat | GE per GE | Feed conversion for pig meat |
| Productivity\|Roughage share\|Ruminant meat and dairy | GE per GE | Share of roughage in ruminant feed |
| Productivity\|Pasture share\|Ruminant meat and dairy | GE per GE | Share of pasture in ruminant feed |

## Author

Benjamin Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- reportFeed()
  } # }

```
