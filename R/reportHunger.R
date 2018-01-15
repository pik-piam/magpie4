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



reportHunger<-function(gdx){
  warning("should be calibrated=TRUE")
  out<-mbind(
    setNames(Hunger(gdx,level="regglo",calibrated = FALSE,share = FALSE),
             "Food Supply|Calorie Supply|Undernourished (Mio People)"),
    setNames(Hunger(gdx,level="regglo",calibrated = FALSE,share = TRUE),
             "Food Supply|Calorie Supply|Share of population undernourished (People/People)")
  )
  return(out)
}