# sdgIndicator

Helper function that renames SDG data according to a fixed naming
scheme.

## Usage

``` r
sdgIndicator(indicatorName, unit, data)
```

## Arguments

- indicatorName:

  The name of the SDG indicator variable (e.g., "SDG\|SDG02\|Food
  availability")

- unit:

  The name of the unit to be used (e.g., "kcal/cap/day")

- data:

  A magpie object containing the actual indicator data

## Value

Returns a magpie object with the correct name
