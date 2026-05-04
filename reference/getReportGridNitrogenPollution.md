# getReportGridNitrogenPollution

Reports nutrient surplus indicators as well as exceedance of the
critical nitrogen surplus at the grid level

## Usage

``` r
getReportGridNitrogenPollution(gdx, reportOutputDir = NULL, scenario = NULL)
```

## Arguments

- gdx:

  gdx file

- reportOutputDir:

  a folder name for the output to be written to. If NULL the report is
  not saved to disk, and only returned to the calling function.

- scenario:

  the name of the scenario used. If NULL the report is not saved to
  disk, and only returned to the calling function.

## Value

A list of MAgPIE objects containing the reports

## Author

Michael Crawford

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- getReportGridNitrogenPollution()
  } # }
```
