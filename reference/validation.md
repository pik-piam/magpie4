# validation

Create Validation pdf from MAgPIE output and corresponding
validation.mif

## Usage

``` r
validation(
  gdx,
  hist,
  file = "validation.pdf",
  runinfo = NULL,
  clusterinfo = NULL,
  debug = FALSE,
  reportfile = NULL,
  scenario = NULL,
  getReport = NULL,
  ...
)
```

## Arguments

- gdx:

  GDX file

- hist:

  Validation data. All formats allowed which can be converted to quitte
  (including characters containing the path to a mif file)

- file:

  a file name the output pdf

- runinfo:

  (optional) Rdata object with run information

- clusterinfo:

  (optional) RDS file or vector containing mapping information on
  0.5degree between regions and cluster

- debug:

  Switch to activate or deactivate debug mode

- reportfile:

  file name to which a backup of the magpie reporting should be written
  (file ending should be ".mif"). No report written if set to NULL or if
  report is already provided via getReport!

- scenario:

  scenario name used inside reportfile. Not used if reportfile is NULL.

- getReport:

  the return value of the `getReport` function. Can be provided if
  available to reduce overall runtime.

- ...:

  additional arguments supplied to the validationpdf function

## Author

Jan Philipp Dietrich

## Examples

``` r
  if (FALSE) { # \dontrun{
    validation("fulldata.gdx","validation.mif",filter="Yield")
  } # }
```
