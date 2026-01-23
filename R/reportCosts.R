#' @title reportCosts
#' @description reports MAgPIE costs
#' @export
#' @param gdx GDX file
#' @return consumption value as MAgPIE object Unit: see names
#' @author Florian Humpenoeder
#' @examples
#'   \dontrun{
#'     x <- reportCosts(gdx)
#'   }
#'
#' @section Cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs | million US$2017/yr | Total production costs
#' Costs\|+\|Input Factors | million US$2017/yr | Costs for input factors (labor, capital, materials)
#' Costs\|+\|Land Conversion | million US$2017/yr | Costs for land conversion
#' Costs\|+\|Transport | million US$2017/yr | Transport costs
#' Costs\|+\|TC | million US$2017/yr | Technology costs (research and development)
#' Costs\|+\|GHG Emissions | million US$2017/yr | Costs from GHG emission pricing
#' Costs\|MainSolve w/o GHG Emissions | million US$2017/yr | Total costs excluding GHG emission costs
#' @md


reportCosts <- function(gdx, level = "regglo") {
  costsData <- costs(gdx, level = level, sum = FALSE)
  getNames(costsData) <- paste0("Costs|+|", getNames(costsData))

  x <- mbind(setNames(dimSums(costsData, dim = 3), "Costs"), costsData)
  x <- mbind(x, setNames(dimSums(costsData[, , c("Costs|GHG Emissions", "Costs|Reward for Afforestation"),
                                           invert = TRUE],
                                 dim = 3),
                         "Costs|MainSolve w/o GHG Emissions"))
  getNames(x) <- paste0(getNames(x), " (million US$2017/yr)")

  return(x)
}