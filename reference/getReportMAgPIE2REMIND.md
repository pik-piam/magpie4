# getReportMAgPIE2REMIND

Based on a MAgPIE gdx file, a report is generated containing only the
variables relevant for the coupling with REMIND. Basically a copy of
getReport, but calling less 'reportXY()' functions.

## Usage

``` r
getReportMAgPIE2REMIND(gdx, file = NULL, scenario = NULL)
```

## Arguments

- gdx:

  GDX file

- file:

  A file name the output should be written to using write.report. If
  NULL the report is returned instead as a MAgPIE object.

- scenario:

  Name of the scenario used for the list-structure of a reporting object
  (x\$scenario\$MAgPIE). If NULL the report is returned instead as a
  MAgPIE object.

## Value

A MAgPIE object containing the report.

## Details

Reports are organized with '\|' as level delimiter and summation symbols
for grouping subcategories into entities e.g. for stackplots. Notice the
following hints for the summation symbol placement:

- Every name should just contain one summation symbol (mostly '+').

- The position of the symbol (counted in '\|' from left side) will
  determine the level.

- Every subitem containing the same summation symbol in the same level
  with the same supercategory name will be summed.

- Items without any summation symbol will be silently ignored.

- Items with different summation symbols will be summed up separately.

- In most of the cases a summation symbol will be just placed before the
  last level (counted in '\|' from left side).

- It is helpful to think about which group of items should be stacked in
  a stackplot.

An example how a summation symbol placement could look like:

      Toplevel
      Toplevel|+|Item 1
      Toplevel|+|Item 2
      Toplevel|Item 2|+|Subitem 1
      Toplevel|Item 2|+|Subitem 1
      Toplevel|++|Item A
      Toplevel|++|Item B
      Toplevel|Item ?

## Author

Florian Humpenoeder, David Klein

## Examples

``` r
if (FALSE) { # \dontrun{
x <- getReportMAgPIE2REMIND(gdx)
} # }
```
