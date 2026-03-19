# woodHarvestArea

Reads wood harvest area separated by source (primforest, secdforest,
forestry, other) and age classes from a gdx. The data is on cluster
level and the unit is Mha per year.

## Usage

``` r
woodHarvestArea(gdx)
```

## Arguments

- gdx:

  A fulldata.gdx of a magpie run, usually with endogenous forestry
  enabled

## Value

A magpie object with the following dimensions: region, id, year, source,
ageClass

## Author

Pascal Sauer
