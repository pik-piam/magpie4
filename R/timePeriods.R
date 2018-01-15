#' @title timePeriods
#' @description Calculates MAgPIE time period lengths between each two timesteps 
#' 
#' @export
#' 
#' @param gdx GDX file
#' @return magpie time periods as MAgPIE object as a number of years
#' @author Mishko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- timePeriods(gdx=gdx)
#'   }
#' 

timePeriods <- function(gdx){
  t_steps <- as.integer(substr(readGDX(gdx, "t"),2,5))
  t_periods <- c(1,sapply(seq_along(t_steps[-1]), function(i) t_steps[i+1]-t_steps[i]))
  t_periods <- setYears(as.magpie(t_periods,temporal=T), paste0("y", t_steps))
  
  return(t_periods)
}