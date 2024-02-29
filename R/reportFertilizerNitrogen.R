#' @title reportFertilizerNitrogen
#' @description Reports inorganic nitrogen application on crops
#'
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#'
#' @param gdx GDX file
#' @param level level of output
#' 
#' @author David M Chen
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#' \dontrun{
#' x <- reportFertilizerNitrogen(gdx)
#' }
#'
reportFertilizerNitrogen <- function(gdx, level = "regglo") {

  budget <- NitrogenBudget(gdx, level = level, cropTypes = TRUE )

  out <- collapseNames(budget[, , "fertilizer"])

  out <- reporthelper(out, level_zero_name = "Resources|Nitrogen|Inorganic Fertilizer Application", partly = TRUE )
  out <- summationhelper(out)
  getNames(out) <- paste0(getNames(out), "Mt Nr/yr")

  return(out)
  
  }