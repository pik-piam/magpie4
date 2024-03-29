#' @title reportAnthropometrics
#' @description reports Underweight, Normalweight, Overweight and Obesity as well as body height for males and females
#'
#' @export
#'
#' @param gdx GDX file
#' @param level spatial aggregation: "reg", "glo", "regglo", "iso"
#' @return Magpie object
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' x <- reportBodyweight(gdx)
#' }
#' @importFrom magclass getNames

reportAnthropometrics <- function(gdx,level="regglo") {

  bodyw=bodyweight(gdx,level=level)
  getNames(bodyw) <- paste0("Nutrition|Anthropometrics|People ", getNames(bodyw), " (million people)")

  bodyheight = anthropometrics(gdx=gdx,indicator="bodyheight", age="adults", sex=TRUE,bmi_groups=FALSE, level=level, final=TRUE,calibrated=TRUE)
  bodyheight_F=bodyheight[,,"F"]
  bodyheight_M=bodyheight[,,"m"]
  getNames(bodyheight_F) <- paste0("Nutrition|Anthropometrics|Body height of female adults (cm/capita)")
  getNames(bodyheight_M) <- paste0("Nutrition|Anthropometrics|Body height of male adults (cm/capita)")

  out<-mbind(bodyw,bodyheight_F,bodyheight_M)
  return(out)
}
