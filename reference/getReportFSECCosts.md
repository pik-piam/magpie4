# getReportFSECCosts

Reports cost indicators for the FSEC project

## Usage

``` r
getReportFSECCosts(gdx, reportOutputDir = NULL, scenario = NULL)
```

## Arguments

- gdx:

  a GDX file

- reportOutputDir:

  a folder name for the output to be written to. If NULL the report is
  not saved to disk, and only returned to the calling function.

- scenario:

  the name of the scenario used. If NULL the report is not saved to
  disk, and only returned to the calling function.

## Value

A .csv containing the summed output of reportCostsAccounting on the
region level

## Author

Michael Crawford

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- getReportFSECCosts(gdx)
  } # }
```
