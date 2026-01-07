#' @title reportFactorCostShares
#' @description reports labor and capital cost share out of factor costs from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param type
#' \itemize{
#'   \item "requirements": shares from factor requirements
#'   \item "optimization": cost shares between labor and capital costs in optimization
#'   \item "accounting": cost shares based on accounting of labor and capital costs
#' }
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return  labor and capital cost shares as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportFactorCostShares(gdx)
#' }
#'
#' @section Factor cost share variables:
#' Name | Unit | Meta
#' ---|---|---
#' Factor cost shares optimization\|Crop products\|+\|Labor cost share | % | Labor cost share in crop production
#' Factor cost shares optimization\|Crop products\|+\|Capital cost share | % | Capital cost share in crop production
#' Factor cost shares optimization\|Livestock products\|+\|Labor cost share | % | Labor cost share in livestock production
#' Factor cost shares optimization\|Livestock products\|+\|Capital cost share | % | Capital cost share in livestock production
#' @md

#'
reportFactorCostShares <- function(gdx, type = "optimization", level = "regglo") {


  outKli <- setNames(factorCostShares(gdx, products = "kli", type = type, level = level) * 100, c("Labor", "Capital"))
  outKcr <- setNames(factorCostShares(gdx, products = "kcr", type = type, level = level) * 100, c("Labor", "Capital"))


  if (!is.null(outKcr)) {
    if (type == "requirements") {
      getNames(outKcr) <- paste0("Factor requirement shares|Crop products|+|", getNames(outKcr), 
                                    " requirement share (%)")
      getNames(outKli) <- paste0("Factor requirement shares|Livestock products|+|", getNames(outKli),
                                    " requirement share (%)")
    } else {
      getNames(outKcr) <- paste0("Factor cost shares ", type, "|Crop products|+|", getNames(outKcr), " cost share (%)")
      getNames(outKli) <- paste0("Factor cost shares ", type, "|Livestock products|+|", getNames(outKli),
                                " cost share (%)")
    }
    out <- mbind(outKcr, outKli)
  }

  return(out)
}
