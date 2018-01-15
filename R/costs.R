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

  x <- readGDX(gdx, "ov11_cost_reg", "ov_cost_reg", select = list(type="level"),
               format="first_found", react="silent")
  if (!sum) {
    total <- x

    tmp_cost <- function(gdx,name,label) {
      cost <- readGDX(gdx,name, format="first_found", select=list(type="level"))
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
                tmp_cost(gdx,"ov_cost_cdr","CDR"),
                tmp_cost(gdx,"ov_cost_bioen","Bioenergy"),
                tmp_cost(gdx,"ov_processing_costs","Processing"),
                tmp_cost(gdx,"ov_cost_landtax","Land Tax"))

    x <- mbind(x)

    #check
    diff <- round(sum(total - dimSums(x,dim=3)),6)
    if(diff > 0) warning("Total costs and sum over detailed costs disagree. Check if all cost terms are included in costs.R function.")
  }
  #aggregate
  x <- superAggregate(x, aggr_type = "sum", level = level,crop_aggr=sum)

  out(x,file)
}
