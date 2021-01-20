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
  p32_land <- readGDX(gdx,"p32_land","p32_land_fore",react = "quiet")
  if(!is.null(p32_land)) {
    #expand p32_land for MAgPIE 4.0
    if(dim(p32_land)[3] == 122) p32_land <- collapseNames(p32_land[,,"after"])
    if(names(dimnames(p32_land))[[3]] == "ac") {
      p32_land_orig <- p32_land
      p32_land <- add_dimension(p32_land,dim=3.1,add = "type32",nm = c("aff","ndc","plant"))
      p32_land[,,"aff"] <- 0
      p32_land[,,"ndc.acx"] <- 0
      p32_land[,,"plant"][,,head(getNames(p32_land,dim=2),-1)] <- 0
      if(abs(sum(p32_land_orig-dimSums(p32_land,dim=3.1))) > 0.1) warning("Differences in p32_land detected")
      names(dimnames(p32_land))[1] <- "j"
    }
  } else {
    ac <- readGDX(gdx,"ac")
    #static forestry module realization
    x <- collapseNames(land(gdx,level="cell",types = "forestry"))
    p32_land <- new.magpie(getCells(x),getYears(x),c(ac),fill = 0)
    p32_land <- add_dimension(p32_land,dim=3.1,add = "type32",nm = c("aff","ndc","plant"))
    p32_land[,,"plant.acx"] <- x
  }

  x=gdxAggregate(gdx,p32_land,to=level,absolute=TRUE)
  
  out(x,file)
}
