# getReportFSECCropDiversityGrid

Reports grid cell level crop diversity for the FSEC project

## Usage

``` r
getReportFSECCropDiversityGrid(gdx, reportOutputDir = NULL, scenario = NULL)
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

A list of MAgPIE objects containing the reports

## Author

Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
x <- getReportFSECCropDiversityGrid(gdx)
} # }
```
