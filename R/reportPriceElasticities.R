
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
  out<-mbind(
    setNames(PriceElasticities(gdx,level="regglo",calibrated = TRUE,products="kfo"),
             "Food Supply|PriceElasticities|Total Calories (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = TRUE,products="kst"),
             "Food Supply|PriceElasticities|Staples (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = TRUE,products="kli"),
             "Food Supply|PriceElasticities|Livestock Products (%/%)"),
    setNames(PriceElasticities(gdx,level="regglo",calibrated = TRUE,products="others"),
             "Food Supply|PriceElasticities|Vegetables, Fruits and Nuts (%/%)")
  )
  return(out)
}
