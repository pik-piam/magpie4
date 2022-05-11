#' @title lastIter
#' @description Returns the value of a parameter in the last iteration
#'
#' @export
#'
#' @param gdx GDX file
#' @param param Parameter to be returned
#' @param secondlast if TRUE, reads the secondlast iteration. For MAgPIE results, usually there is no last iteration as the food demand model reaches convergence before MAgPIE starts.
#'
#' @return magpie object
#' @author Benjamin Leon Bodirsky
#' @importFrom gdx readGDX
#' @examples
#'
#'   \dontrun{
#'     x <- lastIter(gdx)
#'   }
#'

lastIter<-function(gdx,param,secondlast=FALSE){

  allvalue = readGDX(gdx,param, format="first_found", react="silent")
  if(any(getSets(allvalue)=="iter15",na.rm=TRUE)){
    value=collapseNames(allvalue[,,"iter1"])

    if (secondlast) {
      iternumbers = readGDX(gdx,"p15_iteration_counter")-1
      iternumbers[iternumbers<1]=1
      lastiter=readGDX(gdx,"iter15")[iternumbers]
    } else {
      lastiter=readGDX(gdx,"iter15")[readGDX(gdx,"p15_iteration_counter")]
    }
    for(t in 1:nyears(allvalue)) {
      value[,t,] = allvalue[,t,lastiter[t]]
    }
  } else { # downwards compatbility
    value=allvalue
  }

  return(value)
}
