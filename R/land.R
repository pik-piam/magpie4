#' @title land
#' @description reads land out of a MAgPIE gdx file
#' 
#' @importFrom magclass mbind2 fulldim read.magpie
#' @importFrom lucode path
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param types NULL or a vector of strings. If NULL, all land types are used. Options are "crop", "past", "forestry", "primforest","secdforest, "urban" and "other"
#' @param subcategories NULL or vector of strings. If NULL, no subcategories are returned. Meaningful options are "forestry", "secdforest" and "other"
#' @param sum determines whether output should be land-type-specific (FALSE) or aggregated over all types (TRUE).
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return land as MAgPIE object (Mha)
#' @author Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Leon Bodirsky
#' @seealso \code{\link{reportLandUse}}
#' @examples
#' 
#'   \dontrun{
#'     x <- land(gdx)
#'   }
#' 

land <- function(gdx, file=NULL, level="reg", types=NULL, subcategories=NULL, sum=FALSE, spamfiledirectory="") {
  if(level=="grid"){
    x <- read.magpie(path(spamfiledirectory,"cell.land_0.5.mz"))
    x <- x[,"y1985",,invert=T] # 1985 is currently the year before simulation start. has to be updated later
    x <- add_dimension(x,dim=3.2,add="sub","total")
    if(!is.null(subcategories)){warning("argument subcategories is ignored for cellular data")}
  } else {
    x <- readGDX(gdx,"ov_land","ovm_land", format="first_found",select=list(type="level")) 
    x <- add_dimension(x,dim=3.2,add="sub","total")


    if(!is.null(subcategories)) {
      if("crop" %in% subcategories) {
        crop <- croparea(gdx,product_aggr = FALSE,level="cell")
        crop_nobio <- dimSums(crop[,,c("begr","betr"),invert=TRUE])
        crop_bio <- dimSums(crop[,,c("begr","betr")])
        crop <- mbind(setNames(crop_nobio,"crop.nobio"),setNames(crop_bio,"crop.bio"))
        names(dimnames(crop)) <- names(dimnames(x))
      } else crop <- x[,,"crop"]
      if("past" %in% subcategories) {
        warning("There are no subcatgories for pasture. Returning total pasture area")
        past <- x[,,"past"]
      } else past <- x[,,"past"]
      if("forestry" %in% subcategories) {
  #     if (length(unlist(strsplit(names(dimnames(forestry_check))[3],"\\."))) == 3) {
        forestry <- add_dimension(readGDX(gdx,"ov32_land",select=list(type="level")),dim=3.1,add="land","forestry")
        if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")) | names(dimnames(forestry))[[3]]=="land.type32.ac")){
          forestry <- dimSums(forestry,dim = 3.3)
        } 
        if(round(sum(x[,,"forestry.total"] - dimSums(forestry,dim=3.2)),7) != 0) warning("Forestry: Total and sum of subcategory land types diverge! Check your GAMS code!")
      } else forestry <- x[,,"forestry"]
      if("primforest" %in% subcategories) {
        warning("There are no subcatgories for primforest Returning total primforest area")
        primforest <- x[,,"primforest"]
      } else primforest <- x[,,"primforest"]
      if("secdforest" %in% subcategories) {
        secdforest <- add_dimension(readGDX(gdx,"ov35_secdforest",select=list(type="level")),dim=3.1,add="land","secdforest")
        if(round(sum(x[,,"secdforest.total"] - dimSums(secdforest,dim=3.2)),7) != 0) warning("secdforest: Total and sum of subcategory land types diverge! Check your GAMS code!")
      } else secdforest <- x[,,"secdforest"]
      if("urban" %in% subcategories) {
        warning("There are no subcatgories for urban land. Returning total urban area")
        urban <- x[,,"urban"]
      } else urban <- x[,,"urban"]
      if("other" %in% subcategories) {
        other <- add_dimension(readGDX(gdx,"ov35_other",select=list(type="level")),dim=3.1,add="land","other")
        if(round(sum(x[,,"other.total"] - dimSums(other,dim=3.2)),7) != 0) warning("Other: Total and sum of subcategory land types diverge! Check your GAMS code!")
      } else other <- x[,,"other"]
      x <- mbind2(crop,past,forestry,primforest,secdforest,urban,other)
    }
  }
  if(is.null(x)) {
    warning("Land area cannot be calculated as land data could not be found in GDX file! NULL is returned!")
    return(NULL)
  }

  if(!is.null(types)) x <- x[,,types]
  
  x=gdxAggregate(gdx,x,to=level,absolute=TRUE,spamfiledirectory = spamfiledirectory)
  if(sum) {
    x<-dimSums(x,dim=c(3.1,3.2))
  } else x<-collapseNames(x)
  if(all(is.null(getNames(x)))){getNames(x)<-paste0(types,collapse = " ")} ## for netcdf files
  out(x,file)
}
