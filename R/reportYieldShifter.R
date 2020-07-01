#' @title reportYieldShifter
#' @description Reports the Crop model input yield shifter
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param baseyear baseyear for the yield shifter. Also fixes land patterns for aggregation to baseyear.
#' @param relative relative or absolute changes to baseyear
#' @param spamfiledirectory for gridded outputs: magpie output directory which containts the spamfiles for disaggregation
#' @return crop yield as MAgPIE object (unit depends on attributes)
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{reportYieldShifter}}
#' @examples
#' 
#'   \dontrun{
#'     x <- reportYieldShifter(gdx)
#'   }
#' 
# gdx=c("C:/bbb/MAgPIE SVN/inputdata/fulldata_kristine2.gdx")
reportYieldShifter <- function(gdx,file=NULL,level="reg",baseyear="y2000",relative=TRUE,spamfiledirectory="") {
  kcr<-readGDX(gdx,"kcr")
  t<-readGDX(gdx,"t")
  yield_input <- readGDX(gdx,"i14_yields_calib","i14_yields",format="first_found")[,t,kcr]
  yield_input2 <- readGDX(gdx,"f14_yields")[,t,kcr]
  
  constant_baseyear<-function(baseyear,...) {
    out <- croparea(...)
    out[,,]<-setYears(out[,baseyear,],NULL)
    return(out)
  }
  yield_input<- gdxAggregate(x=yield_input,weight = 'constant_baseyear',to = level,absolute = FALSE, gdx = gdx,spamfiledirectory = spamfiledirectory,product_aggr=FALSE,water_aggr=FALSE,baseyear=baseyear)
  
  if(relative==TRUE){
    yield_input_rel <- yield_input/setYears(yield_input[,baseyear,],NULL)
    # NA to relatively low starting yields
    #threshold=yield_input
    #threshold[,,]= setYears(yield_input[,baseyear,],NULL)
    #yield_input_rel[threshold<0.1]=NA
  } else {
    yield_input_rel <- yield_input - setYears(yield_input[,baseyear,],NULL)
  }
  

  
  getNames(yield_input_rel,dim=1)<-reportingnames(getNames(yield_input_rel,dim=1))
  getNames(yield_input_rel,dim=2)<-reportingnames(getNames(yield_input_rel,dim=2))
  getNames(yield_input_rel) = paste0("Productivity|Climate Change Yield Shifter|",sub("\\.", "|", getNames(yield_input_rel))," (Index ",baseyear,"=1)")
  
  write.magpie(yield_input_rel,file_name = "yield_change_rel.nc")
  
  return(yield_input)
}
