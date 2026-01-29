#' @title reportSDG3
#' @description reports all SDG indicators relevant for SDG3 - Health
#'
#' @export
#'
#' @param gdx GDX file
#' @return MAgPIE object
#' @author Benjamin Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportSDG3(gdx)
#'   }
#'
#'
#' @section SDG3 Health variables:
#' Name | Unit | Meta
#' ---|---|---
#' SDG\|SDG03\|Prevalence of overweight | million | Population with overweight BMI
#' SDG\|SDG03\|Prevalence of obesity | million | Population with obesity
#' SDG\|SDG03\|Prevalence of overweight\|Children | million | Children under 5 with overweight BMI
#' SDG\|SDG03\|Prevalence of obesity\|Children | million | Children under 5 with obesity
#' SDG\|SDG03\|Consumption of alcohol | kcal/cap/day | Daily per-capita caloric intake from alcohol
#' @md
reportSDG3 <- function(gdx, level = "regglo") {
  population <- population(gdx, level = "iso", bmi_groups = TRUE, sex = TRUE, age = TRUE)
  bodyweight <- bodyweight(gdx, level = level, population = population)
  bodyweight_underaged <- bodyweight(gdx,
                                     level = level,
                                     age = "underaged",
                                     population = population)

  return(mbind(
    sdgIndicator("SDG|SDG03|Prevalence of overweight", "million",
                 bodyweight[, , "overweight"]),
    sdgIndicator("SDG|SDG03|Prevalence of obesity", "million",
                 bodyweight[, , "obese"]),
    sdgIndicator("SDG|SDG03|Prevalence of overweight|Children", "million",
                 bodyweight_underaged[, , "overweight"]),
    sdgIndicator("SDG|SDG03|Prevalence of obesity|Children", "million",
                 bodyweight_underaged[, , "obese"]),
    sdgIndicator("SDG|SDG03|Consumption of alcohol", "kcal/cap/day",
                 Kcal(gdx, level = level, products = "alcohol"))
  ))
}
