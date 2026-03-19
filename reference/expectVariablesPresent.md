# expectVariablesPresent

Checks whether a set of expected variable names is present in a MAgPIE
report object. Variable names are normalized in that summation symbols
are removed.

## Usage

``` r
expectVariablesPresent(report, variableNames)
```

## Arguments

- report:

  A MAgPIE object containing the report to check.

- variableNames:

  A character vector of variable names that are expected to be present
  in `report`.

## Value

`NULL` invisibly. Issues a warning if expected variables are not
present.

## See also

Other Infrastructure: [`metadata_comments()`](metadata_comments.md),
[`out()`](out.md), [`tryList()`](tryList.md),
[`tryReport()`](tryReport.md)

## Author

Patrick Rein
