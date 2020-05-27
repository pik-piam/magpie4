#' @title costs
#' @description reads costs entering the objective function from a MAgPIE gdx file
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param sum total costs (TRUE) or detailed costs (FALSE)
#' @return A MAgPIE object containing the goal function costs [million US$05]
#' @author Jan Philipp Dietrich, Markus Bonsch, Misko Stevanovic, Florian Humpenoeder
#' @importFrom gdx readGDX out
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- costs(gdx)
#'   }
#'

costs <- function(gdx,file=NULL,level="reg",sum=TRUE) {

  
  #
  if(suppressWarnings(is.null(readGDX(gdx,"p13_oall_cost_tc")))){
    x <- readGDX(gdx, "ov11_cost_reg", "ov_cost_reg", select = list(type="level"),
                 format="first_found", react="silent")
    if (!sum) {
      total <- x
      
      tmp_cost <- function(gdx,name,label) {
        cost <- readGDX(gdx,name, format="first_found", select=list(type="level"),react = "quiet")
        if(is.null(cost)) return(NULL)
        cost <- dimSums(cost,dim=3)
        cost <- superAggregate(cost, aggr_type = "sum", level="reg")
        dimnames(cost)[[3]] <- label
        return(cost)
      }
      
      x <- list(tmp_cost(gdx,"ov_cost_prod","Input Factors"),
                tmp_cost(gdx,"ov_cost_landcon","Land Conversion"),
                tmp_cost(gdx,"ov_cost_transp","Transport"),
                tmp_cost(gdx,"ov_tech_cost","TC"),
                tmp_cost(gdx,"ov_nr_inorg_fert_costs","N Fertilizer"),
                tmp_cost(gdx,"ov_p_fert_costs","P Fertilizer"),
                tmp_cost(gdx,"ov_emission_costs","GHG Emissions"),
                tmp_cost(gdx,"ov_reward_cdr_aff","Reward for Afforestation")*-1,
                tmp_cost(gdx,"ov_maccs_costs","MACCS"),
                tmp_cost(gdx,"ov_cost_AEI","AEI"),
                tmp_cost(gdx,"ov_cost_trade","Trade"),
                tmp_cost(gdx,"ov_cost_fore","Forestry"),
                tmp_cost(gdx,"ov_cost_bioen","Bioenergy"),
                tmp_cost(gdx,c("ov_cost_processing","ov_processing_costs"),"Processing"),
                tmp_cost(gdx,"ov_costs_overrate_cropdiff","Punishment overrated cropland difference"),
                tmp_cost(gdx,"ov_bioenergy_utility","Reward for producing bioenergy"),
                tmp_cost(gdx,"ov_processing_substitution_cost","Substitution processing"),
                tmp_cost(gdx,"ov_costs_additional_mon","Punishment cost for additionally transported monogastric livst_egg"),
                tmp_cost(gdx,"ov_cost_land_transition","Land transition matrix"),
                tmp_cost(gdx,"ov_peatland_cost","Peatland"),
                tmp_cost(gdx,"ov_peatland_emis_cost","Peatland GHG emisssions")
      )
      
      if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
        x[[length(x)+1]] <-  tmp_cost(gdx,"ov_cost_natveg","Natural Vegetation")
      }
      
      x <- mbind(x)
      
      #check
      diff <- round(sum(total - dimSums(x,dim=3)),6)
      if(diff > 0) warning("Total costs and sum over detailed costs disagree. Check if all cost terms are included in costs.R function.")
    }
    
  }else{ 
    tmp_cost <- function(gdx,name,label) {
      
      if(label %in% c("TC","Input Factors","AEI","Land Conversion")){
        cost <- readGDX(gdx,name, format="first_found",react = "quiet")
      }else{
        cost <- readGDX(gdx,name, format="first_found", select=list(type="level"),react = "quiet")
      }
      
      if(is.null(cost)) return(NULL)
      cost <- dimSums(cost,dim=3)
      cost <- superAggregate(cost, aggr_type = "sum", level="reg")
      dimnames(cost)[[3]] <- label
      return(cost)
    }
    
    x <- list(tmp_cost(gdx,"p38_ovcosts","Input Factors"),
              tmp_cost(gdx,"pc39_ovcost_land","Land Conversion"),
              tmp_cost(gdx,"ov_cost_transp","Transport"),
              tmp_cost(gdx,"p13_oall_cost_tc","TC"),
              tmp_cost(gdx,"ov_nr_inorg_fert_costs","N Fertilizer"),
              tmp_cost(gdx,"ov_p_fert_costs","P Fertilizer"),
              tmp_cost(gdx,"ov_emission_costs","GHG Emissions"),
              tmp_cost(gdx,"ov_reward_cdr_aff","Reward for Afforestation")*-1,
              tmp_cost(gdx,"ov_maccs_costs","MACCS"),
              tmp_cost(gdx,"pc41_ovcost_AEI","AEI"),
              tmp_cost(gdx,"ov_cost_trade","Trade"),
              tmp_cost(gdx,"ov_cost_fore","Forestry"),
              tmp_cost(gdx,"ov_cost_bioen","Bioenergy"),
              tmp_cost(gdx,c("ov_cost_processing","ov_processing_costs"),"Processing"),
              tmp_cost(gdx,"ov_costs_overrate_cropdiff","Punishment overrated cropland difference"),
              tmp_cost(gdx,"ov_bioenergy_utility","Reward for producing bioenergy"),
              tmp_cost(gdx,"ov_processing_substitution_cost","Substitution processing"),
              tmp_cost(gdx,"ov_costs_additional_mon","Punishment cost for additionally transported monogastric livst_egg"),
              tmp_cost(gdx,"ov_cost_land_transition","Land transition matrix"),
              tmp_cost(gdx,"ov_peatland_cost","Peatland"),
              tmp_cost(gdx,"ov_peatland_emis_cost","Peatland GHG emisssions")
    )
    
    
    if(suppressWarnings(!is.null(readGDX(gdx,"fcostsALL")))){
      x[[length(x)+1]] <-  tmp_cost(gdx,"ov_cost_natveg","Natural Vegetation")
    }
    
    x <- mbind(x)
    
    if (sum) {
      x<-dimSums(x,dim=3)
    }
    
    
  }

  #aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level,crop_aggr=sum)

  out(x,file)
}
