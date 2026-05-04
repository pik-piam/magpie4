# getReportFableScenathon

Collects outputs from MAgPIE runs for FABLE Scenathon.

## Usage

``` r
getReportFableScenathon(gdx, file = NULL, iso = NULL)
```

## Arguments

- gdx:

  a GDX file

- file:

  a file name the output should be written to using write.report. If
  \`NULL\` the report is returned instead as a MAgPIE object. For the
  easier reporting in Scenathon tabs, a .csv file extension is
  recommenended.

- iso:

  country/region selection. Default \`NULL\`, i.e. all \`regglo\`
  reporting

## Author

Miodrag Stevanovic

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- getReportFableScenathon(gdx, file = "magpie2scenathon.csv", iso = "IND")
  } # }
```
