#' @title reportGrasslandYields
#' @description reportGrasslandYields
#' 
#' @importFrom magpiesets reporthelper summationhelper
#' @export
#' 
#' @param gdx GDX file
#' @return yield as MAgPIE object (Mt DM/ha)
#' @author Marcos Alves
#' @examples
#' 
#'   \dontrun{
#'     x <- reportGrasslandYields(gdx)
#'   }
#' 

reportGrasslandYields <- function(gdx) {
  grass_yields <- NULL
  x <- NULL
  grass_areas <- NULL
  grass_yld <- NULL
  
  try({grass_yld <- grassyld(gdx)})
  try({grass_areas <- readGDX(gdx, "ov31_grass_area", format = "simplest")[, , list("type" = "level")]})
  # try({grass_yld <- readGDX(gdx, "i31_grass_yields", format = "simplest")})
  # try({tau <- readGDX(gdx, "ov_tau", format = "simplest")})
  
  if(!is.null(grass_yld)) {
    # grass_areas <- collapseNames(grass_areas)
    # grass_yld <- collapseNames(grass_yld)
    # past_prod <- grass_areas * grass_yld
    # 
    # past_prod_reg <- gdxAggregate(gdx, past_prod, to = "regglo", absolute = T)
    # grass_areas_reg <- gdxAggregate(gdx, grass_areas, to = "regglo", absolute = T)
    # 
    # grass_yields <- past_prod_reg/grass_areas_reg
    # grass_yields[is.nan(grass_yields) | is.infinite(grass_yields)] <- 0
    # 
    # x <- setNames(grass_yields, paste0("Productivity|Yield|+|", reportingnames(getNames(grass_yields)), " (t DM/ha)"))
    
    # grass_yld <- collapseNames(grass_yld)
    # tau <- gdxAggregate(gdx, tau, to = "cell", absolute = F)
    # grass_yld <- grass_yld[,getYears(tau),]
    # grass_yld[,,"pastr"] <- tau[,,"pastr.level"] * grass_yld[,,"pastr"]

    grass_yields <- gdxAggregate(gdx, grass_yld, weight = grass_areas, to = "regglo", absolute = F)
    x <- setNames(grass_yields, paste0("Productivity|Yield|+|", reportingnames(getNames(grass_yields)), " (t DM/ha)"))
  } else {
    x <- "Disabled (No separate grassland yields)"
  }
  return(x)
} 
