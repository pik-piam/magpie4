#' @title reportOverallCosts
#' @description Calculates Overall costs [million US$17] including total investment costs
#'
#' @export
#'
#' @param gdx GDX file
#' @param level level of aggregation of output
#' @return A MAgPIE object containing the overall costs [million US$17] including total investment costs
#' @author Edna J. Molina Bacca
#' @importFrom magclass mbind dimSums collapseNames
#' @importFrom luscale superAggregate
#' @examples
#'
#'   \dontrun{
#'     x <- reportOverallCosts(gdx)
#'   }
#'

reportOverallCosts<- function(gdx,level="reg"){

    tmp_cost <- function(gdx,name,label) {
        cost <- readGDX(gdx,name, format="first_found", select=list(type="level"),react = "quiet")
        if(is.null(cost)) return(NULL)
        cost <- dimSums(cost,dim=3)
        cost <- superAggregate(cost, aggr_type = "sum", level="reg")
        dimnames(cost)[[3]] <- label
        return(cost)
      }

      x <- list(tmp_cost(gdx,"ov_cost_transp","Transport"),
                tmp_cost(gdx,"ov_nr_inorg_fert_costs","N Fertilizer"),
                tmp_cost(gdx,"ov_p_fert_costs","P Fertilizer"),
                tmp_cost(gdx,"ov_emission_costs","GHG Emissions"),
                tmp_cost(gdx,"ov_reward_cdr_aff","Reward for Afforestation")*-1,
                tmp_cost(gdx,"ov_maccs_costs","MACCS"),
                tmp_cost(gdx,"ov_cost_trade","Trade"),
                tmp_cost(gdx,"ov_cost_fore","Forestry"),
                tmp_cost(gdx,"ov_cost_timber","Timber production"),
                tmp_cost(gdx,"ov_cost_bioen","Bioenergy"),
                tmp_cost(gdx,c("ov_cost_processing","ov_processing_costs"),"Processing"),
                tmp_cost(gdx,"ov_costs_overrate_cropdiff","Punishment overrated cropland difference"),
                tmp_cost(gdx,"ov_bioenergy_utility","Reward for producing bioenergy"),
                tmp_cost(gdx,"ov_processing_substitution_cost","Substitution processing"),
                tmp_cost(gdx,"ov_costs_additional_mon","Punishment cost for additionally transported monogastric livst_egg"),
                tmp_cost(gdx,"ov_cost_land_transition","Land transition matrix"),
                tmp_cost(gdx,"ov_peatland_emis_cost","Peatland GHG emisssions"),
                tmp_cost(gdx, "ov_cost_packaging", "Wholesale Costs"))

      if(suppressWarnings(is.null(readGDX(gdx,"ov_cost_inv")))){

        y <- list(tmp_cost(gdx,"ov_cost_prod","Input Factors"),
                  tmp_cost(gdx,"ov_cost_landcon","Land Conversion"),
                  tmp_cost(gdx,"ov_tech_cost","TC"),
                  tmp_cost(gdx,"ov_cost_AEI","AEI"),
                  tmp_cost(gdx,"ov_peatland_cost","Peatland")
        )

      }else{

        years<-getYears(readGDX(gdx,"ov_cost_prod")[,,"level"])
        interest_rate<-readGDX(gdx,"pm_interest")[,years,]

        if(suppressWarnings(is.null(readGDX(gdx," ov58_peatland_cost_annuity")))){
          annuity_peatland<-0
        }else{
          annuity_peatland<-dimSums(readGDX(gdx," ov58_peatland_cost_annuity")[,,"level"],dim=3)
        }

        if(suppressWarnings(is.null(readGDX(gdx,"s38_depreciation_rate")))){
          factor_overall_sticky<-1
        }else{
          depreciation<-readGDX(gdx,"s38_depreciation_rate")
          factor_overall_sticky<-(1-depreciation)*(interest_rate/(1+interest_rate))+depreciation
        }


        investment_costs<-tmp_cost(gdx,"ov_cost_inv","Input Factors")/factor_overall_sticky
        factor_overall<-interest_rate/(1+interest_rate)

        y <- list(tmp_cost(gdx,"ov_cost_prod","Input Factors")+investment_costs,
                  tmp_cost(gdx,"ov_cost_landcon","Land Conversion")/factor_overall,
                  tmp_cost(gdx,"ov_tech_cost","TC")/factor_overall,
                  tmp_cost(gdx,"ov_cost_AEI","AEI")/factor_overall,
                  tmp_cost(gdx,"ov_peatland_cost","Peatland")+annuity_peatland/factor_overall
        )
      }

      x<-append(x,y)

      x <- mbind(x)

  if (sum) {
      x<-dimSums(x,dim=3)
    }


  #aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level,crop_aggr=sum)

  return(x)
}
