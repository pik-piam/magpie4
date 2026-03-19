# tryList

Internal support function to run a list of reportings in a
[`tryReport`](tryReport.md) environment.

## Usage

``` r
tryList(..., gdx, level = "regglo")
```

## Arguments

- ...:

  report function to be run

- gdx:

  gdx file to report from

- level:

  spatial level (either "regglo" for region+global or "iso" for ISO
  countries)

## Value

A list of magpie objects (successful reports) or NULL (failed reports)

## See also

[`tryReport`](tryReport.md), [`reportResult`](reportResult.md)

Other Infrastructure:
[`expectVariablesPresent()`](expectVariablesPresent.md),
[`metadata_comments()`](metadata_comments.md), [`out()`](out.md),
[`tryReport()`](tryReport.md)

## Author

Jan Philipp Dietrich
