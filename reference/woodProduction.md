# woodProduction

Reads roundwood and fuelwood production/harvest data separated by source
(primforest, secdforest, forestry, other) from a gdx. The data is on
cluster level and the unit is Petagram (= mio. t) dry matter per year
(Pg DM yr-1).

## Usage

``` r
woodProduction(gdx)
```

## Arguments

- gdx:

  A fulldata.gdx of a magpie run, usually with endogenous forestry
  enabled

## Value

A magpie object with the following dimensions: region, id, year, source,
woodType

## Author

Pascal Sauer
