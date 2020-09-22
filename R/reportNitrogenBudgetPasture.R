#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @export
#' 
#' @param gdx GDX file
#' @param grid if TRUE, disaggregate to grid level
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

reportNitrogenBudgetPasture<-function(gdx, grid=FALSE, dir=".",spamfiledirectory=""){
  dir <- getDirectory(dir,spamfiledirectory)
  
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
    
    out<-NitrogenBudgetPasture(gdx,level="grid",dir=dir)
    getNames(out)<-reportingnames(getNames(out))
    
  } else {warning("grid has to be boolean")}
  
  return(out)
}