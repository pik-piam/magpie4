# getReportINMS

Puts together a report for the INMS project based on a MAgPIE gdx file

## Usage

``` r
getReportINMS(
  gdx,
  file = NULL,
  scenario = NULL,
  filter = c(2, 7),
  detail = TRUE,
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

- filter:

  Modelstat filter. Here you have to set the modelstat values for which
  results should be used. All values for time steps in which the
  modelstat is different or for which one of the previous modelstats
  were different are set to NA.

- detail:

  Crop specific (TRUE) or aggregated outputs (FALSE)

- ...:

  additional arguments for write.report. Will only be taken into account
  if argument "file" is not NULL.

## Value

A MAgPIE object containing the report in the case that "file" is NULL.

## Author

Benjamin Bodirsky, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
x <- getReport(gdx)
} # }
```
