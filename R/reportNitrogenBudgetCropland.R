#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @export
#' 
#' @param gdx GDX file
#' @param include_emissions TRUE also divides the N surplus into different emissions
#' @param grid grid provides outputs on grid level of 0.5 degree
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds or spam) disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#' 
#' @examples
#'   \dontrun{
#'     x <- reportNitrogenBudgetCropland(gdx)
#'   }
#' 

reportNitrogenBudgetCropland<-function(gdx,include_emissions=FALSE,grid=FALSE,dir=".",spamfiledirectory=""){
  dir <- getDirectory(dir,spamfiledirectory)
  if(grid==FALSE){
    budget<-NitrogenBudget(gdx,level="regglo",include_emissions=include_emissions)
    budget[,,"som"] = -budget[,,"som"]
    
    
    all<-getNames(budget)
    withdrawaltypes<-c("harvest","ag","bg")
    balancetypes<-c("surplus","som","balanceflow")
    if(include_emissions){
      emissiontypes = c("n2o_n","nh3_n","no2_n","no3_n")
    } else {
      emissiontypes = NULL
    }
    inputtypes<-setdiff(all,c(withdrawaltypes,balancetypes,emissiontypes))
    
    tmp<-budget[,,inputtypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Inputs|+|",reportingnames(getNames(tmp)))
    inputs<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Inputs"),
      tmp
    )
    
    tmp<-budget[,,withdrawaltypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Withdrawals|+|",reportingnames(getNames(tmp)))
    withdrawals<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Withdrawals"),
      tmp
    )
    
    tmp<-budget[,,balancetypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Cropland Budget|Balance|+|",reportingnames(getNames(tmp)))
    balance<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Cropland Budget|Balance"),
      tmp
    )
    
    if(include_emissions){
      tmp <- budget[,,emissiontypes]
      getNames(tmp) <- paste0("Resources|Nitrogen|Cropland Budget|Balance|Nutrient Surplus|",reportingnames(getNames(tmp)))
      emissions <- tmp
    } else {emissions <- NULL}
    
    out<-mbind(
      inputs,
      withdrawals,
      balance,
      emissions
    )
    
    getNames(out)<-paste0(getNames(out)," (Mt Nr/yr)")
    
  } else {
    
    out<-NitrogenBudget(gdx,level="grid",dir=dir,include_emissions=include_emissions)
    getNames(out)<-reportingnames(getNames(out))
    
    
    x <- metadata_comments(x=out,unit="Mt Nr/yr", description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",comment="",note="")
    
    #withMetadata(TRUE)
    #getMetadata(a,type="unit")<-"Mt Nr/yr"
    #withMetadata(FALSE)
  }

  return(out)
}