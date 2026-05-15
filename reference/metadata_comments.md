# metadata_comments

set metadata comments to magpie4 objects

## Usage

``` r
metadata_comments(x, unit, description, comment, note)
```

## Arguments

- x:

  magpie object (magpie4)

- unit:

  provide unit

- description:

  provide short description

- comment:

  optional comment

- note:

  optional note

## Value

vector of comments following order of input (unit, description, comment,
note - further: origin, creation data)

## See also

Other Infrastructure:
[`expectVariablesPresent()`](expectVariablesPresent.md),
[`out()`](out.md), [`tryList()`](tryList.md),
[`tryReport()`](tryReport.md)

## Author

Benjamin Bodirsky, Jannes Breier

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- metadata_comments(x,unit,description,comment,note)
  } # }
```
