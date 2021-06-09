#' @title protectedArea
#' @description reads protectedArea out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @param sum sum over land pools (default = FALSE)
#' @details protected areas in primforest, secdforest and other land 
#' @return protected area in Mha
#' @author Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- protectedArea(gdx)
#'   }
#' 

protectedArea <- function(gdx, file=NULL, level="cell", sum=FALSE){
  
  #read in protected areas
  a <- readGDX(gdx,"p35_save_natveg",react="silent")
  if (is.null(a)) {
    primforest <- setNames(readGDX(gdx,"p35_save_primforest",react="silent"),"primforest")
    secdforest <- setNames(readGDX(gdx,"p35_save_secdforest",react="silent"),"secdforest")
    other <- setNames(readGDX(gdx,"p35_save_other",react="silent"),"other")
    a <- mbind(primforest,secdforest,other)
    }
  
  names(dimnames(a))[1] <- "j"
  
  #sum
  if (sum) a <- dimSums(a,dim=3.1)
  
  #aggregate over regions
  if (level != "cell") a <- superAggregate(a, aggr_type = "sum", level = level,na.rm = FALSE)
  
  out(a,file)
}
