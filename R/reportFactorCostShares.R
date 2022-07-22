#' @title reportFactorCostShares
#' @description reports labor and capital cost share out of factor costs from MAgPIE results
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo"
#' @return  labor and capital cost shares as MAgPIE object
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' x <- reportFactorCostShares(gdx)
#' }
#'
reportFactorCostShares <- function(gdx, level = "regglo") {

  outKli <- setNames(factorCostShares(gdx, products = "kli", level = level) * 100, c("Labor", "Capital"))
  outKcr <- setNames(factorCostShares(gdx, products = "kli", level = level) * 100, c("Labor", "Capital"))

  if (!is.null(outKcr)) {
    getNames(outKcr) <- paste0("Factor cost shares|Crop products|+|", getNames(outKcr), " cost share (%)")
    getNames(outKli) <- paste0("Factor cost shares|Livestock products|+|", getNames(outKli), " cost share (%)")
    out <- mbind(outKcr, outKli)
  }

  return(out)
}
