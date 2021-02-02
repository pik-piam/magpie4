#' @title GrowingStockFRA
#' @description reads woody growing stock out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Growing stock for producing woody materials consist of growing stock from plantations (forestry), secondary and primary forest as well as other land (natveg)
#' @return Growing stock in m3 per ha
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#'
#' \dontrun{
#' x <- GrowingStockFRA(gdx)
#' }
#'
GrowingStockFRA <- function(gdx, file = NULL, level = "reg") {
  if (level == "reg") {
   gs_relative_plantations <- setNames(readGDX(gdx,"p32_updated_gs_reg"),"plantations")
   gs_relative_natfor <- setNames(readGDX(gdx,"p35_updated_gs_natfor"),"natfor")
   a <- mbind(gs_relative_natfor,gs_relative_plantations)
  } else {
    message("ERROR - wrong regions")
    a <- NULL
  }
  out(a, file)
}
