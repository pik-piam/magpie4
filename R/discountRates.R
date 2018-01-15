#' @title discountRates
#' @description reads discount rates from a MAgPIE gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @return A MAgPIE object containing discount rates used in the model
#' @author Xiaoxi Wang
#' @examples
#' 
#'   \dontrun{
#'     x <- discountRates(gdx)
#'   }
#'

discountRates<- function(gdx,file=NULL,level="reg") {
      x <- readGDX(gdx,"pm_interest",format="first_found")
      wt <- income(gdx,per_capita = FALSE)
      wt <- apply(wt[,(1:3),],1,mean)
      wt <- as.magpie(wt)
      x <- superAggregate(x,aggr_type="weighted_mean",level=level,weight=wt)
      out(x,file)
}