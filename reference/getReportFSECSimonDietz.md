# getReportFSECSimonDietz

Collects reports for Simon Dietz' social welfare function analysis

## Usage

``` r
getReportFSECSimonDietz(gdx, reportOutputDir = NULL, scenario = NULL)
```

## Arguments

- gdx:

  GDX file

- reportOutputDir:

  a folder name for the output to be written to. If NULL the report is
  not saved to disk, and only returned to the calling function.

- scenario:

  the name of the scenario used. If NULL the report is not saved to
  disk, and only returned to the calling function.

## Value

A list of reports

## Author

Michael Crawford

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- getReportFSECSimonDietz()
  } # }
```
