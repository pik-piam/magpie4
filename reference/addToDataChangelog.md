# addToDataChangelog

Prepend data from the given report to the changelog.

## Usage

``` r
addToDataChangelog(
  report,
  changelog,
  versionId,
  years = changelogYears(),
  variables = changelogVariables(),
  ...,
  maxEntries = 15,
  roundDigits = 2
)
```

## Arguments

- report:

  data.frame as obtained by readRDS("report.rds")

- changelog:

  Path to the changelog file

- versionId:

  The model version identifier, e.g. a release number like 4.9.1 or a
  date like 2025-02-01

- years:

  For which years the variables should be read and put into the
  changelog

- variables:

  named vector of which variables to read from the report

- ...:

  Reserved for future expansion.

- maxEntries:

  The maximum number of versionIds to keep in the changelog, the oldest
  one is removed first.

- roundDigits:

  Numbers are rounded to this many decimal places before being written
  to the changelog.

## Value

Invisibly, the written changelog as data.frame

## See also

Other Data Changelog: [`changelogVariables()`](changelogVariables.md),
[`changelogYears()`](changelogYears.md)

## Author

Pascal Sauer
