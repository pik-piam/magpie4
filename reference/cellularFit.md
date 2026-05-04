# cellular fit

cellular fit/error/bias calculations at regional and global level

## Usage

``` r
cellularFit(
  gdx,
  file = NULL,
  level = "cell",
  statistic = "MAE",
  variable = "land",
  dataset = "LUH3",
  water_aggr = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  level at which the regional and global bias should be calculated.
  Options "cell" or "grid"

- statistic:

  R2, MAE, MPE (mean percentage error - bias), MAPE (mean absolute
  percentage error)

- variable:

  variable to be evaulated: land (land types) or crop (crop types)

- dataset:

  dataset to compare with. LUH3 only option for variable land. LUH3 and
  MAPSPAM for the crop variable.

- water_aggr:

  if irrigation types for crops should be agregated or not

## Value

returns selected statistic at regglo level for the historical part of
the time horizon

## Author

Edna J. Molina Bacca, Patrick v. Jeetze

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- cellularFit(gdx)
  } # }
```
