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
reportCostsMACCS <- function(gdx) {

  maccsCosts <- costsMACCS(gdx, level = "regglo")

  if (!is.null(maccsCosts)) {
      getNames(maccsCosts) <- c("Costs Optimization|MACCS|+|Labor costs (million US$2017/yr)",
                                "Costs Optimization|MACCS|+|Capital costs (million US$2017/yr)")
  }

  return(maccsCosts)
}
