#' @title reportNitrogenBudgetCropland
#' @description Reports the Nitrogen Budgets of Croplands for future MAgPIE projections
#' 
#' @importFrom magpiesets reportingnames
#' @export
#' 
#' @param gdx GDX file
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#' 
#' @examples
#'   \dontrun{
#'     x <- reportNitrogenBudgetCropland(gdx)
#'   }
#' 

reportNitrogenBudgetPasture<-function(gdx){
  out<-NitrogenBudgetPasture(gdx,level="regglo")
  names_x<-reportingnames(getNames(out))
  names(names_x)<-NULL
  getNames(out)<-paste0("Resources|Nitrogen|Pasture Budget|",names_x," (Mt Nr)")
  return(out)
}