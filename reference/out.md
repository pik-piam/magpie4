# out

Function to safely returns parameters. Function returns either the
output or writes it to a file. Please use this function when you write
own GDX output functions.

## Usage

``` r
out(x,file)
```

## Arguments

- x:

  an object that can be converted to a MAgPIE object

- file:

  file name of a file it should be written to. NULL, if x should be
  returned instead to be written to a file.

## Value

NULL or x as MAgPIE object

## See also

Other Infrastructure:
[`expectVariablesPresent()`](expectVariablesPresent.md),
[`metadata_comments()`](metadata_comments.md),
[`tryList()`](tryList.md), [`tryReport()`](tryReport.md)

## Author

Jan Philipp Dietrich
