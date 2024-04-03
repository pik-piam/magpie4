#' @title reportCostsWholesale
#' @description Reads data to calculate wholesale costs 
#' @export
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global),
#' "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing values related with costs wholesale trade [million US$05/tDM]
#' @author David M Chen
#' @examples
#' \dontrun{
#' x <- reportCostsWholesale(gdx)
#' }
#'
reportCostsWholesale <- function(gdx, level = "regglo") {

  #read packaging costs
  x <- costsWholesale(gdx, level = level)

  if (!is.null(x)) {
 
    x <- magpiesets::reporthelper(x, partly = TRUE, detail = FALSE, level_zero_name = "Costs|Wholesale Costs")
    x <- magpiesets::summationhelper(x, excludeLevels = 1)

    getNames(x) <- paste0(getNames(x), " (million US$05/yr)")
  }

    return(x)
}
