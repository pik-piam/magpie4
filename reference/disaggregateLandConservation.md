# disaggregateLandConservation

Read land conservation data from a fulldata.gdx and disaggregate to high
resolution (0.5 deg).

## Usage

``` r
disaggregateLandConservation(gdx, cfg, mapping, wdpaHr, conservationPrioHr)
```

## Arguments

- gdx:

  character, path to a fulldata.gdx

- cfg:

  list, config data usually obtained with gms::loadConfig("config.yml")

- mapping:

  dataframe, must include columns cell and cluster

- wdpaHr:

  magclass, World Database on Protected Areas data in high resolution

- conservationPrioHr:

  magclass, high resolution data on conservation priority areas

## Value

magclass, high resolution land conservation area

## Author

Patrick v. Jeetze, Pascal Sauer
