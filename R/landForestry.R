#' @title landForestry
#' @description reads and compiles forestry land subcategories from a  MAgPIE gdx file
#'
#' @importFrom magclass getNames getYears collapseNames add_dimension
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return land as MAgPIE object (Mha)
#' @author Florian Humpenoeder
#' @seealso \code{\link{reportLandUse}}
#' @examples
#'
#'   \dontrun{
#'     x <- land(gdx)
#'   }
#'

landForestry <- function(gdx, file=NULL, level="reg") {

  # detailed forestry land area: aff, ndc, plant
  ov32_land <- readGDX(gdx, "ov32_land", select = list(type = "level"))
  if(!is.null(ov32_land)) {
    #expand ov32_land for MAgPIE 4.0
    if(dim(ov32_land)[3] == 122) ov32_land <- collapseNames(ov32_land[,,"after"])
    if(names(dimnames(ov32_land))[[3]] == "ac") {
      p32_land_orig <- ov32_land
      ov32_land <- add_dimension(ov32_land,dim=3.1,add = "type32",nm = c("aff","ndc","plant"))
      ov32_land[,,"aff"] <- 0
      ov32_land[,,"ndc.acx"] <- 0
      ov32_land[,,"plant"][,,head(getNames(ov32_land,dim=2),-1)] <- 0
      if(abs(sum(p32_land_orig-dimSums(ov32_land,dim=3.1))) > 0.1) warning("Differences in ov32_land detected")
      names(dimnames(ov32_land))[1] <- "j"
    }
  } else {
    ac <- readGDX(gdx,"ac")
    #static forestry module realization
    x <- collapseNames(land(gdx,level="cell",types = "forestry"))
    ov32_land <- new.magpie(getCells(x),getYears(x),c(ac),fill = 0)
    ov32_land <- add_dimension(ov32_land,dim=3.1,add = "type32",nm = c("aff","ndc","plant"))
    ov32_land[,,"plant.acx"] <- x
  }

  x=gdxAggregate(gdx,ov32_land,to=level,absolute=TRUE)

  out(x,file)
}
