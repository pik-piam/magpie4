#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @export
#' 
#' @param gdx GDX file
#' @param grid if TRUE, disaggregate to grid level
#' @param spamfiledirectory spamfiledirectory for cellular results
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#' 
#' @examples
#'   \dontrun{
#'     x <- reportNitrogenBudgetCropland(gdx)
#'   }
#' 

reportNitrogenBudgetPasture<-function(gdx, grid=FALSE, spamfiledirectory=""){
  
  if (grid==FALSE){
    
    budget<-NitrogenBudgetPasture(gdx,level="regglo")
    #budget[,,"som"] = -budget[,,"som"]
    
    all<-getNames(budget)
    withdrawaltypes<-c("harvest")
    balancetypes<-c("surplus")
    inputtypes<-setdiff(setdiff(all,withdrawaltypes),balancetypes)
    
    tmp<-budget[,,inputtypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Inputs|+|",reportingnames(getNames(tmp)))
    inputs<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Inputs"),
      tmp
    )
    
    tmp<-budget[,,withdrawaltypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Withdrawals|+|",reportingnames(getNames(tmp)))
    withdrawals<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Withdrawals"),
      tmp
    )
    
    tmp<-budget[,,balancetypes]
    getNames(tmp)<-paste0("Resources|Nitrogen|Pasture Budget|Balance|+|",reportingnames(getNames(tmp)))
    balance<-mbind(
      setNames(dimSums(tmp,dim=3),"Resources|Nitrogen|Pasture Budget|Balance"),
      tmp
    )
    
    out<-mbind(
      inputs,
      withdrawals,
      balance
    )
    
    getNames(out)<-paste0(getNames(out)," (Mt Nr/yr)")
    
  } else if (grid == TRUE){
    budget<-NitrogenBudgetPasture(gdx,level="cell")
    
    out <-  gdxAggregate(gdx = gdx,x = budget,weight = 'production',to = "grid",
                                 absolute = TRUE,spamfiledirectory = spamfiledirectory,
                                 attributes = "nr",products = "pasture",product_aggr = TRUE)
    getNames(out)<-reportingnames(getNames(out))
    
  } else {warning("grid has to be boolean")}
  
  return(out)
}