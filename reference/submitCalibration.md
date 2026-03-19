# submitCalibration

Submits Calibration Factors of current run to calibration archive.
Currently covers calibration factors for yields and land conversion
costs. This is useful to make runs more comparable to each other. The
function can be also used as part of a script running a collection of
runs.

## Usage

``` r
submitCalibration(
  name,
  file = c("modules/14_yields/input/f14_yld_calib.csv",
    "modules/39_landconversion/input/f39_calib.cs3"),
  archive = "/p/projects/landuse/data/input/calibration"
)
```

## Arguments

- name:

  name under which the calibration should be stored. Should be as
  self-explaining as possible. The total file name has the format
  calibration\_\<name\>\_\<date\>.tgz.

- file:

  path to a f14_yld_calib.csv and f39_calib.cs3 (older version
  f39_calib.csv) file (in this order). Alternatively a fulldata.gdx file
  containing the calibration factors can be used. Supported file types
  are "csv", "cs3" and "gdx".

- archive:

  path to the archive the calibration factors should be stored

## Value

file name of the stored calibration factors (useful for scripts in which
you might want to re-use a calibration setting at a later stage again)

## Author

Jan Philipp Dietrich, Florian Humpenoeder, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
fname <- submitCalibration("TestCalibration", file = "fulldata.gdx")
} # }
```
