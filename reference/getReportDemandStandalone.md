# getReportDemandStandalone

Puts together a report based on a MAgPIE gdx file

## Usage

``` r
getReportDemandStandalone(
  gdx,
  file = NULL,
  scenario = NULL,
  detail = FALSE,
  ...
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.report. If
  NULL the report is returned instead as a MAgPIE object.

- scenario:

  Name of the scenario used for the list-structure of a reporting object
  (x\$scenario\$MAgPIE). If NULL the report is returned instead as a
  MAgPIE object.

- detail:

  Crop specific (TRUE) or aggregated outputs (FALSE)

- ...:

  additional arguments for write.report. Will only be taken into account
  if argument "file" is not NULL.

## Value

A MAgPIE object containing the report in the case that "file" is NULL.

## Author

Florian Humpenoeder

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- getReportDemandStandalone(gdx)
  } # }
```
