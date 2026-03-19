# tryReport

Internal support function to run a reporting in a try environment and
properly report problems if something goes wrong without stopping the
further processing in case of an error.

## Usage

``` r
tryReport(report, gdx, level = "regglo", env = parent.frame())
```

## Arguments

- report:

  report function to be run

- gdx:

  gdx file to report from

- level:

  spatial level (either "regglo" for region+global, "iso" for
  country-level, or the file of a mapping file).

- env:

  environment to evaluate the report in

## Value

A named list with information on the outcome of the report (success,
error, validationError, warning)

## See also

[`reportResult`](reportResult.md)

Other Infrastructure:
[`expectVariablesPresent()`](expectVariablesPresent.md),
[`metadata_comments()`](metadata_comments.md), [`out()`](out.md),
[`tryList()`](tryList.md)

## Author

Jan Philipp Dietrich
