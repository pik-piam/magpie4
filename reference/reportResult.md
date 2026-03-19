# reportResult

Create a named list representing the outcome of a report

## Usage

``` r
reportResult(resultType, message, reportExpr, elapsed, result = NULL)
```

## Arguments

- resultType:

  Character string for the result type

- message:

  Character string with the result message

- reportExpr:

  The expression containing the call to the report function

- elapsed:

  The elapsed time for the report in seconds

- result:

  The result magpie object or NULL

## Value

A named list with report result information
