#' @title reportNitrogenBudgetNonagland
#' @description Reports the Nitrogen Budgets of non-agricultural lands for future MAgPIE projections
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
#'     x <- reportNitrogenBudgetNonagland(gdx)
#'   }
#' 

reportNitrogenBudgetNonagland<-function(gdx, grid=FALSE, dir=".", spamfiledirectory=""){
  dir <- getDirectory(dir,spamfiledirectory)
  if (grid==FALSE){
    
    budget<-NitrogenBudgetNonagland(gdx,level="reg",dir=dir)
    
  } else if (grid == TRUE){
    
    
    budget<-NitrogenBudgetNonagland(gdx,level="grid",dir=dir)
    out<-dimSums(budget,dim=3.2)
    
    getNames(out)<-reportingnames(getNames(out))
    
    #out=metadata_comments(out = out, unit = "Mt Nr/yr", description = "Nitrogen budget for non-agricultural land, sum over all land types excluding pasture and cropland",note = "")
    
  } else {warning("grid has to be boolean")}
  
  return(out)
}