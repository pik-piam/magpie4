# bodyweight

Calculates the prevalence of underweight, normalweight, overweight
(excluding obesity) and obesity. For more detailed body mass
classifications see functions population or anthropometrics.

## Usage

``` r
bodyweight(
  gdx,
  level = "reg",
  age = FALSE,
  sex = FALSE,
  share = FALSE,
  population = NULL
)
```

## Arguments

- gdx:

  GDX file

- level:

  Level of regional aggregation; "iso" ISO country codes, "reg"
  (regional), "glo" (global)

- age:

  if TRUE, demand is scaled down to age-groups and sex using food
  requirements

- sex:

  if FALSE, female and male are aggregated, if sex, results are divided
  into males and females

- share:

  if TRUE, data is provided by BMI group

- population:

  population information from GDX. Can be provided to speed up
  calculation process. Will be read from GDX, if not provided.

## Value

MAgPIE object with mio people or share of people in each weight category

## Details

Demand definitions are equivalent to FAO Food supply categories

## Author

Benjamin Leon Bodirsky

## Examples

``` r

  if (FALSE) { # \dontrun{
    x <- bodyweight(gdx)
  } # }
```
