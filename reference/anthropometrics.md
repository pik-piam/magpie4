# anthropometrics

Calculates anthropometic indicators from the food demand model

## Usage

``` r
anthropometrics(
  gdx,
  indicator = "bodyheight",
  age = "adults",
  sex = FALSE,
  bmi_groups = FALSE,
  level = "iso",
  final = TRUE,
  file = NULL,
  calibrated = TRUE
)
```

## Arguments

- gdx:

  GDX file

- indicator:

  bodyheight, bodyweight, bodyweight_healthy, BMI(Body Mass Index) or
  PAL (physical activity level)

- age:

  if TRUE, demand is scaled down to age-groups and sex using food
  requirements

- sex:

  if FALSE, female and male are aggregated, if sex, results are divided
  into males and females

- bmi_groups:

  if TRUE, data is provided by BMI group

- level:

  Level of regional aggregation; "iso" ISO country codes, "reg"
  (regional), "glo" (global)

- final:

  final results or preliminary results (the latter are the ones magpie
  uses for optimization before last iteration with demand model)

- file:

  a file name the output should be written to using write.magpie

- calibrated:

  if TRUE, uses the calibrated intake estimates for bodyweight
  estimation

## Value

bodyweight (kg), bodyheight (cm), BMI or PAL as magpie objects

## Details

Demand definitions are equivalent to FAO Food supply categories

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- anthropometrics(gdx)
  } # }
```
