#' @title reportCostInputs
#' @description reports MAgPIE costs
#' 
#' @export
#' 
#' @param gdx GDX file
#' 
#' @return Magpie object associated with overall costs and value of production
#' @author Edna J. Molina Bacca
#' @examples
#' 
#'   \dontrun{
#'     x <- reportCostInputs(gdx)
#'   }
#' @importFrom magclass getNames

reportCostInputs<-function(gdx){
  
  if(type_aux<-suppressWarnings(is.null(readGDX(gdx,"ov_cost_inv")))){
    
    stop("Input costs including overall investments only available for the sticky realization")
  
 }else{
   
   cost_annuity<-CostInputFactors(gdx,type="overall",level="regglo")
   getNames(cost_annuity) <- paste0(getNames(cost_annuity)," (million US$05/yr)")
    
  }
  
  return(cost_annuity)
}