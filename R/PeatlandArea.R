#' @title PeatlandArea
#' @description reads peatland area out of a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any aggregation level defined in superAggregate. In addition "climate" for the 3 climate regions tropical, temperate and boreal is available.
#' @param sum sum over land types TRUE (default) or FALSE
#' @details Intact, degraded and rewettet peatland area
#' @return Peatland area in Mha
#' @author Florian Humpenoeder
#' @importFrom magclass dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- PeatlandArea(gdx)
#'   }

PeatlandArea <- function(gdx, file=NULL, level="cell", sum=TRUE){

  a <- readGDX(gdx,"ov58_peatland","ov58_peatland_man",select=list(type="level"),react = "silent")
  if(!is.null(a)) {
    if(names(dimnames(a))[[3]] == "man58.land58") {
      if(sum) {
        a <- dimSums(a,dim=c(3.2))
        a[,,"degrad"] <- a[,,"degrad"] + collapseNames(a[,,"unused"])
        a <- a[,,"unused",invert=TRUE]
        b <- readGDX(gdx,"ov58_peatland_intact",select=list(type="level"))
        getNames(b) <- "intact"
        a <- mbind(a,b)
      } else {
        b <- readGDX(gdx,"ov58_peatland_intact",select=list(type="level"))
        getNames(b) <- "intact.intact"
        a <- mbind(a,b)
      }
    } else if (names(dimnames(a))[[3]] == "land58") {
      if(sum) {
        b <- new.magpie(getCells(a),getYears(a),c("degrad","intact","rewet"))
        b[,,"degrad"] <- dimSums(a[,,c("crop","past","forestry","unused","peatExtract")],dim=3)
        b[,,"intact"] <- dimSums(a[,,c("intact")],dim=3)
        b[,,"rewet"] <- dimSums(a[,,c("rewetted")],dim=3)
        a <- b
      } else{
        a <- a
      }
    }

    if(level == "climate") {
      map_cell_clim <- readGDX(gdx,"p58_mapping_cell_climate")
      a <- dimSums(map_cell_clim*a,dim=1)
    } else if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  } else a <- NULL

  out(a,file)
}
