#' @title reportSDG2
#' @description reports all SDG indicators relevant for SD2 - Hunger
#'
#' @export
#'
#' @param gdx GDX file
#' @param level An aggregation level for the spatial dimension. Can be any level
#' available via superAggregateX.
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG2(gdx)
#'   }
#'
#'
#' @section SDG2 Hunger variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG02\|Prevalence of underweight | million | Population with underweight BMI
#' SDG\|SDG02\|Prevalence of underweight\|Children | million | Children under 5 with underweight BMI
#' SDG\|SDG02\|Food availability | kcal/cap/day | Daily per-capita caloric availability
#' SDG\|SDG02\|Food expenditure share | income | Share of income spent on food (value added)
#' SDG\|SDG02\|Agricultural primary product expenditure share | income | Share of income spent on agricultural primary products
#' SDG\|SDG02\|Agricultural commodity price index wrt 2020 | 1 | Food price index relative to 2020 baseline
#' SDG\|SDG02\|Prevalence of obesity\|Children | million | Children under 5 with obesity
#' SDG\|SDG02\|Investment in AgR&D | million US$2017/yr | Investment in agricultural research and development
#' @md
reportSDG2 <- function(gdx, level = "regglo") {
  population <- population(gdx, level = "iso", bmi_groups = TRUE, sex = TRUE, age = TRUE)
  bodyweight <- bodyweight(gdx, level = level, population = population)
  bodyweight_underaged <- bodyweight(gdx, level = level, age = "underaged", population = population)

  indicatorname <- "SDG|SDG02|Prevalence of undernourishment"
  unit <- "million"
  #missing

  indicatorname <- "SDG|SDG02|Malnutrition under five"
  unit <- "million"
  #missing

  return(mbind(
    sdgIndicator("SDG|SDG02|Prevalence of underweight", "million",
                 bodyweight[, , "underweight"]),
    sdgIndicator("SDG|SDG02|Prevalence of underweight|Children", "million",
                 bodyweight_underaged[, , "underweight"]),
    sdgIndicator("SDG|SDG02|Food availability", "kcal/cap/day",
                 Kcal(gdx, level = level)),
    sdgIndicator("SDG|SDG02|Food expenditure share", "income",
                 FoodExpenditureShare(gdx, level = level, valueAdded = TRUE)),
    sdgIndicator("SDG|SDG02|Agricultural primary product expenditure share", "income",
                 FoodExpenditureShare(gdx, level = level, valueAdded = FALSE)),
    sdgIndicator("SDG|SDG02|Agricultural commodity price index wrt 2020", "1",
                 priceIndex(gdx, level = level, baseyear = "y2020", products = "kfo") / 100),
    sdgIndicator("SDG|SDG02|Prevalence of obesity|Children", "million",
                 bodyweight_underaged[, , "obese"]),
    sdgIndicator("SDG|SDG02|Investment in AgR&D", "million US$2017/yr",
                 costs(gdx, sum = FALSE, level = level)[, , "TC"])
  ))

}
