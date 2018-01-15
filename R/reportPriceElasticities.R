
#' @title reportHunger
#' @description Calculates the share of people living in hunger.
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return magpie object with hunger (mio people) or hunger share 
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#'   \dontrun{
#'     x <- reportHunger(gdx)
#'   }
#' 

reportPriceElasticities <- function(gdx){
  warning("should be calibrated=TRUE")
  out<-mbind(
    setNames(PriceElasticities(gdx,level="regglo",calibrated = FALSE,product_aggr = TRUE),
             "Food Supply|PriceElasticities|Total Calories (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = FALSE,product_aggr = FALSE)[,,"maiz"],
             "Food Supply|PriceElasticities|Staples (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = FALSE,product_aggr = FALSE)[,,"livst_chick"],
             "Food Supply|PriceElasticities|Livestock Products (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = FALSE,product_aggr = FALSE)[,,"others"],
             "Food Supply|PriceElasticities|Vegetables, Fruits and Nuts (%/%)")
  )
  return(out)
}
