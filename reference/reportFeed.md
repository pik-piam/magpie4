# reportFeed

reportes feed demand by animal type

## Usage

``` r
reportFeed(gdx, detail = TRUE, level = "regglo")
```

## Arguments

- gdx:

  GDX file

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- level:

  An aggregation level for the spatial dimension. Can be any level
  available via superAggregateX.

## Value

feed demand as MAgPIE object (Mt DM)

## Feed demand variables

|                                          |          |                                          |
|------------------------------------------|----------|------------------------------------------|
| Name                                     | Unit     | Meta                                     |
| Demand\|Feed\|++\|Feed for Ruminant meat | Mt DM/yr | Feed demand for ruminant meat production |
| Demand\|Feed\|++\|Feed for Dairy         | Mt DM/yr | Feed demand for dairy production         |
| Demand\|Feed\|++\|Feed for Pig meat      | Mt DM/yr | Feed demand for pig meat production      |
| Demand\|Feed\|++\|Feed for Poultry meat  | Mt DM/yr | Feed demand for poultry meat production  |
| Demand\|Feed\|++\|Feed for Eggs          | Mt DM/yr | Feed demand for egg production           |
| Demand\|Feed\|++\|Feed for Aquaculture   | Mt DM/yr | Feed demand for aquaculture              |

## Author

Isabelle Weindl

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- reportFeed()
  } # }
```
