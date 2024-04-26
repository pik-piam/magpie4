#' @title reportNitrogenBudgetNonagland
#' @description Reports the Nitrogen Budgets of non-agricultural lands for future MAgPIE projections
#'
#' @importFrom magpiesets reportingnames
#' @export
#'
#' @param gdx GDX file
#' @param grid if TRUE, disaggregate to grid level
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @author Benjamin Leon Bodirsky
#' @seealso
#' \code{\link{NitrogenBudget}}
#'
#' @examples
#'   \dontrun{
#'     x <- reportNitrogenBudgetNonagland(gdx)
#'   }
#'

reportNitrogenBudgetNonagland<-function(gdx, grid=FALSE, dir="."){

  if (grid==FALSE){

    budget<-NitrogenBudgetNonagland(gdx,level="reg",dir=dir)
    budget<-dimSums(budget,dim=3.2)

    surplus = budget[,,"surplus"]
    inputs = budget[,,c("fixation_freeliving","deposition")]

    helper=function(prefix,x) {
      total = dimSums(x,dim=3)
      getNames(total)<-paste0(prefix," (Mt Nr/yr)")
      getNames(x)<-reportingnames(getNames(x))
      getNames(x)<-paste0(prefix,"|+|",getNames(x), " (Mt Nr/yr)")
      return(mbind(total,x))
    }
    surplus = helper(prefix="Resources|Nitrogen|Non-Agricultural Land Budget|Balance",x=surplus)
    inputs  = helper(prefix="Resources|Nitrogen|Non-Agricultural Land Budget|Inputs",x=inputs)
    out<-mbind(surplus, inputs)
    out<-mbind(out,setItems(dimSums(out,dim=1),dim=1,"GLO"))

  } else if (grid == TRUE){

    budget<-NitrogenBudgetNonagland(gdx,level="grid",dir=dir)
    out<-dimSums(budget,dim=3.2)
    getNames(out)<-reportingnames(getNames(out))
  }else {warning("grid has to be boolean")}

  #out=metadata_comments(out = out, unit = "Mt Nr/yr", description = "Nitrogen budget for non-agricultural land, sum over all land types excluding pasture and cropland",note = "")
  return(out)
}
