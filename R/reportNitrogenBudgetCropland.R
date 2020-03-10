#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @importFrom moinput toolFertilizerDistribution
#' @export
#' 
#' @param gdx GDX file
#' @param grid grid provides outputs on grid level of 0.5 degree
#' @param spamfiledirectory directory with data on 0.5 degree required for downscaling
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#' 
#' @examples
#'   \dontrun{
#'     x <- reportNitrogenBudgetCropland(gdx)
#'   }
#' 

reportNitrogenBudgetCropland<-function(gdx,grid=FALSE,spamfiledirectory=""){
  if(grid==FALSE){
    budget<-NitrogenBudget(gdx,level="regglo")
    budget[,,"som"] = -budget[,,"som"]
    
    
    all<-getNames(budget)
    withdrawaltypes<-c("harvest","ag","bg")
    balancetypes<-c("surplus","som","balanceflow")
    inputtypes<-setdiff(setdiff(all,withdrawaltypes),balancetypes)
    
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
    
    out<-mbind(
      inputs,
      withdrawals,
      balance
    )
    getNames(out)<-paste0(getNames(out)," (Mt Nr/yr)")
    
  } else {
    
    out<-NitrogenBudget(gdx,level="grid",spamfiledirectory=spamfiledirectory)
    getNames(out)<-reportingnames(getNames(out))
    
    
    x <- metadata_comments(x=x,unit="Mt Nr/yr", description="Total land area in its primary land cover categories. Other includes non-forest natural vegetation like savannas.",comment="",note="")
    
    #withMetadata(TRUE)
    #getMetadata(a,type="unit")<-"Mt Nr/yr"
    #withMetadata(FALSE)
  }

  return(out)
}