# getReportGridINMS

Generates and saves a list of reports relevant to the INMS context

## Usage

``` r
getReportGridINMS(
  gdx,
  reportOutputDir = NULL,
  scenario = NULL,
  filter = c(2, 7),
  version = "v13"
)
```

## Arguments

- gdx:

  GDX file

- reportOutputDir:

  Directory in which the reports are to be saved. If NULL, a list of
  reports (MAgPIE objects) is returned instead

- scenario:

  Name of the scenario used for the list-structure of a reporting object
  (x\$scenario\$MAgPIE). If NULL a list of reports ( MAgPIE objects) is
  returned instead.

- filter:

  Modelstat filter. Here you have to set the modelstat values for which
  results should be used. All values for time steps in which the
  modelstat is different or for which one of the previous modelstats
  were different are set to NA.

- version:

  Version number for this analysis

## Value

A list of reports (MAgPIE objects)

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Michael Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
x <- getReportGridINMS(gdx)
} # }
```
