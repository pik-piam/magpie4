#' @title reportCostsMACCS
#' @description reports MAgPIE mitigation costs disaggregated into labor and capital
#'
#' @param gdx GDX file
#' @return magpie object with mitigation costs
#' @author Debbora Leip
#' @examples
#'
#'   \dontrun{
#'     x <- reportCostsMACCS(gdx)
#'   }
#'
#' @section MACC cost variables:
#' Name | Unit | Meta
#' ---|---|---
#' Costs\|MACCS\|+\|Labor costs | million US$2017/yr | Labor costs for MACC implementation
#' Costs\|MACCS\|+\|Capital costs | million US$2017/yr | Capital costs for MACC implementation
#' @md
reportCostsMACCS <- function(gdx, level = "regglo") {

  maccsCosts <- costsMACCS(gdx, level = level)

  if (!is.null(maccsCosts)) {
    getNames(maccsCosts) <- c("Costs|MACCS|+|Labor costs (million US$2017/yr)",
                              "Costs|MACCS|+|Capital costs (million US$2017/yr)")
  }

  return(maccsCosts)
}
