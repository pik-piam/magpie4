#' @title reportNitrogenBudgetNonagland
#' @description Reports the Nitrogen Budgets of non-agricultural lands for future MAgPIE projections
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
#'     x <- reportNitrogenBudgetNonagland(gdx)
#'   }
#' 

reportNitrogenBudgetNonagland<-function(gdx, grid=FALSE, spamfiledirectory=""){
  
  if (grid==FALSE){
    
    stop("so far only implemented on grid level")
    
  } else if (grid == TRUE){
    
    
    budget<-NitrogenBudgetNonagland(gdx,level="grid",spamfiledirectory=spamfiledirectory)
    out<-dimSums(budget,dim=3.2)
    
    getNames(out)<-reportingnames(getNames(out))
    
    x=metadata_comments(x = x, unit = "Mt Nr/yr", description = "Nitrogen budget for non-agricultural land, sum over all land types excluding pasture and cropland",note = "")
    
  } else {warning("grid has to be boolean")}
  
  return(out)
}