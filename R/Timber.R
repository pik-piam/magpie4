#' @title Timber
#' @description reads timber demand out of a MAgPIE gdx file
#' 
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "cell", "reg" (regional), "glo" (global), "regglo" (regional and global) or any secdforest aggregation level defined in superAggregate
#' @details Forest demandfor timber production
#' @return Forest demandfor timber production
#' @author Abhijeet Mishra
#' @importFrom gdx readGDX out
#' @importFrom magclass clean_magpie dimSums collapseNames setYears write.magpie setCells
#' @importFrom luscale superAggregate
#' @examples
#' 
#'   \dontrun{
#'     x <- Timber(gdx)
#'   }

Timber <- function(gdx, file=NULL, level="regglo"){
  a <- NULL
  kforestry <- readGDX(gdx,"kforestry")
  if (level %in% c("reg","regglo")){
    f73_volumetric_conversion <- readGDX(gdx,"f73_volumetric_conversion")
    f73_volumetric_conversion <- add_columns(x = f73_volumetric_conversion, addnm = "constr_wood")
    f73_volumetric_conversion[,,"constr_wood"] <- f73_volumetric_conversion[,,"wood"]
    
    ov_supply <- readGDX(gdx, "ov_supply", select=list(type="level"))[,,kforestry] 
    ov_supply <- superAggregate(data = ov_supply,aggr_type = "sum",level = level)
    ov_supply <- add_columns(x = ov_supply, addnm = "constr_wood")
    
    ov_prod <- readGDX(gdx, "ov_prod", select=list(type="level"))[,,kforestry] 
    ov_prod <- superAggregate(data = ov_prod,aggr_type = "sum",level = level)
    ov_prod <- add_columns(x = ov_prod, addnm = "constr_wood")
    
    v73_prod_heaven_timber <- readGDX(gdx,"ov73_prod_heaven_timber", select=list(type="level"))[,,kforestry] 
    v73_prod_heaven_timber <- superAggregate(data = v73_prod_heaven_timber,aggr_type = "sum",level = level)
    v73_prod_heaven_timber <- add_columns(x = v73_prod_heaven_timber, addnm = "constr_wood")
    
    p73_demand_constr_wood <- readGDX(gdx,"p73_demand_constr_wood",react = "silent")
    if(is.null(p73_demand_constr_wood)) {
      p73_demand_constr_wood <- 0
      ov_supply[,,"constr_wood"] <- 0
      ov_prod[,,"constr_wood"] <- 0
      }else {
        p73_demand_constr_wood <- superAggregate(data = p73_demand_constr_wood,level = level, aggr_type = "sum") ## This is regional, we will distribute it to cells based on a simple weight
        ov_supply[,,"constr_wood"] <- p73_demand_constr_wood[,getYears(ov_supply),]
        ov_supply[,,"wood"] <- ov_supply[,,"wood"] - ov_supply[,,"constr_wood"]
        
        if(level=="reg")    constr_wood_share <- ov_supply[,,c("wood","constr_wood")]/dimSums(ov_supply[,,c("wood","constr_wood")],dim=c(1,3))
        if(level=="regglo") constr_wood_share <- ov_supply[,,c("wood","constr_wood")]/dimSums(ov_supply["GLO",,c("wood","constr_wood")],dim=c(1,3))
        
        if(level=="reg") ov_prod[,,"constr_wood"] <- dimSums(ov_prod[,,"wood"],dim=1) * constr_wood_share
          
        if(level=="regglo") ov_prod[,,"constr_wood"] <- dimSums(ov_prod["GLO",,"wood"],dim=1) * constr_wood_share[,,"constr_wood"]
      
        ov_prod[,,"wood"] <- ov_prod[,,"wood"] - ov_prod[,,"constr_wood"]
        }
    
    ov_supply <- ov_supply / f73_volumetric_conversion
    ov_prod <- ov_prod / f73_volumetric_conversion
    v73_prod_heaven_timber <- v73_prod_heaven_timber/f73_volumetric_conversion
    v73_prod_heaven_timber[is.na(v73_prod_heaven_timber)] <- 0
    
    a <- mbind(add_dimension(x = ov_supply,dim = 3.1,nm = "Demand"),
               add_dimension(x = ov_prod,dim = 3.1,nm = "Production"),
               add_dimension(x = v73_prod_heaven_timber, dim = 3.1,nm = "Heaven"))
  } else if (level == "cell"){
    stop("Resolution not recognized. Select reg or regglo as level. NULL returned.")
  }
  
  out(a,file)
}