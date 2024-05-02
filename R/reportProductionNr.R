#' @title reportProductionNr
#' @description reports production in Nr analogous to reportProduction
#'
#' @import magpiesets
#' @export
#'
#' @param gdx GDX file
#' @param detail if detail = FALSE, the subcategories of groups are not reported (e.g. "soybean" within "oilcrops")
#' @return production as MAgPIE object. Unit: see names
#' @author Benjamin Leon Bodirsky
#' @examples
#'
#'   \dontrun{
#'     x <- reportProductionNr(gdx)
#'   }
#'

reportProductionNr <- function(gdx, detail = FALSE) {
  x   <- collapseNames(production(gdx = gdx, level = "regglo", products = readGDX(gdx, "kall"),
                                  product_aggr = FALSE, water_aggr = TRUE, attributes = "nr"),
                       collapsedim = "attributes")
  out <- reporthelper(x = x, dim = 3.1, level_zero_name = "Production Nr", detail = detail)
  getNames(out) <- paste(getNames(out), "(Mt Nr/yr)", sep = " ")
  out <- summationhelper(out)
  return(out)
}
