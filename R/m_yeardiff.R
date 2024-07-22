
#' @title m_yeardiff
#' @description Calculates the parameter m_yeardiff, which is a macro within MAgPIE.
#' @param gdx GDX file
#' @return a magpie object with the length of each timestep
#' @author Benjamin Leon Bodirsky
#' @importFrom magclass new.magpie
#' @export
#' @examples
#'
#'   \dontrun{
#'     x <- m_yeardiff(gdx)
#'   }
#'

m_yeardiff <- function(gdx){
  years=as.numeric(substring(as.vector(readGDX(gdx,"t")),2,5))
  out=new.magpie("GLO",years,"yeardiff")
  out[,,]<-years
  for (i in rev(2:length(years))){
    out[,i,]=out[,i,]-setYears(out[,i-1,],NULL)
  }
  out[,1,]=1
  return(out)
}
