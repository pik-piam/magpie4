# Intake

Calculates the per-capita kcal intake from the food demand model

## Usage

``` r
Intake(
  gdx,
  file = NULL,
  level = "reg",
  calibrated = TRUE,
  pregnancy = FALSE,
  per_capita = TRUE,
  age = FALSE,
  sex = FALSE,
  bmi_groups = FALSE
)
```

## Arguments

- gdx:

  GDX file

- file:

  a file name the output should be written to using write.magpie

- level:

  Level of regional aggregation; "iso" ISO country codes, "reg"
  (regional), "glo" (global)

- calibrated:

  if FALSE, the true regression outputs are used, if TRUE the values
  calibrated to the start years are used

- pregnancy:

  if TRUE, adding the intake requirements for lactation and pregnancy

- per_capita:

  per capita or aggregated for the population

- age:

  if FALSE age and sex is aggregated

- sex:

  if TRUE, data is provided by sex

- bmi_groups:

  if TRUE data is proided by BMI group

## Value

calories as MAgPIE object (unit depends on per_capita: kcal/cap/day
(TRUE), kcal/day (FALSE))

## Details

Demand definitions are equivalent to FAO Food supply categories

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- Intake(gdx)
  } # }
```
