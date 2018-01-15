#' @title land_price
#' @description Calculates MAgPIE MAgPIE land shadow prices based on a gdx file
#' 
#' @export
#' 
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param ignore_lowbound Some shadow prices are positive (see Details), corresponding to a lower bound for that pool. \code{TRUE} sets them to 0. Default value: \code{FALSE}.
#' @param absolute Should the absolute value of all the marginals be taken into calculations? \code{TRUE} (default) of \code{FALSE}. See Details.
#' @param digits rounding accuracy for the output
#' @details The land price is obtained through marginals of the "oq_cropland" constraint. The majority of these marginals are negative values, and a negligible number of them are positive. This is the consequence of the constraint binding either on upper or lower level. The parameter \code{ignore_lowbound} removes all the positive marginals from land price calculation (negligible), and parameter \code{absolute} transforms them into negative values (to be all together reported as positive values at the final calculation).
#' @return A MAgPIE object containing the land shadow prices (US$05/ha).
#' @author Markus Bonsch, Misko Stevanovic
#' @examples
#' 
#'   \dontrun{
#'     x <- land_price(level="regglo", products="kcr")
#'   }
#' 

land_price <- function(gdx, file=NULL, level="reg",ignore_lowbound=FALSE,absolute=TRUE, digits=4) {
  
  # land price (marginal)
  ov_land_p_cell <- readGDX(gdx,"oq30_cropland","oq_cropland", format="first_found")[,,"marginal"]
  if(is.null(ov_land_p_cell)) {
    warning("Land shadow prices cannot be calculated as needed data could not be found in GDX file! 
            NULL is returned!")
    return(NULL)
  }
  
  # ignore positive marginal values
  if(ignore_lowbound) {
    tmp <- array(NA, dim=dim(ov_land_p_cell), dimnames=dimnames(ov_land_p_cell))
    tmp[,,] <- ov_land_p_cell[,,]
    tmp[which(tmp>0)] <- 0
    ov_land_p_cell <- as.magpie(tmp)
  }
  
  # transform positive into negative values
  if(absolute){
    tmp <- array(NA, dim(ov_land_p_cell), dimnames(ov_land_p_cell))
    tmp[,,] <- ov_land_p_cell[,,]
    tmp[which(tmp>0)] <- -tmp[which(tmp>0)]
    ov_land_p_cell <- as.magpie(tmp)
  }
  
  if(level=="cell"){
    land<- as.magpie(-ov_land_p_cell)
  } else{
    ov_area_cell <- land(gdx,level="cell",types="crop",sum=FALSE)
    if(is.null(ov_area_cell)) {
      warning("Land shadow prices cannot be calculated as needed data could not be found in GDX file! 
              NULL is returned!")
      return(NULL)
    }
    dimnames(ov_land_p_cell)[[3]] <- NULL
    dimnames(ov_area_cell)[[3]] <- NULL
    land <- as.magpie(superAggregate(as.magpie(-ov_land_p_cell*ov_area_cell),level=level,aggr_type="sum",crop_aggr=FALSE)/superAggregate(ov_area_cell,level=level,aggr_type="sum",crop_aggr=FALSE))
  }
  dimnames(land)[[3]] <- NULL
  land <- as.magpie(round(land,digits))
  out(land,file)
  }