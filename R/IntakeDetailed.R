#' @title IntakeDetailed
#' @description Calculates detailed or aggregated per-capita kcal intake including exogenous scenarios
#'
#' @export
#'
#' @param gdx GDX file
#' @param file a file name the output should be written to using write.magpie
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param product_aggr aggregate over products or not (boolean)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param spamfiledirectory deprecated. please use \code{dir} instead
#' @details Calculation of kcal food intake is possible for both exogenous diet scenarios and endogenous estimation from food demand model
#' @return Calories as MAgPIE object (unit: kcal/cap/day)
#' @author Isabelle Weindl
#' @importFrom magclass dimSums
#' @examples
#'
#'   \dontrun{
#'     x <- IntakeDetailed(gdx)
#'   }
#'

IntakeDetailed <- function(gdx,
                           file=NULL,
                           level="reg",
                           product_aggr=FALSE,
                           dir=".",
                           spamfiledirectory=""
){

  dir <- getDirectory(dir,spamfiledirectory)

  p15_intake_detail = readGDX(gdx,"p15_intake_detail",react="silent")
  if (length(p15_intake_detail)>0){ # New Realization
    intake_scen=p15_intake_detail
    agg=dimSums(intake_scen)
    # Test
    if(abs(sum(agg)-sum(readGDX(gdx,"p15_intake_total"))>0.001)){warning("intake inconsistent with intake_detail")}
    if(product_aggr){intake_scen <- agg}
  } else {  # Older realization. To be discontinued.
    stop("You use an outdated realization. Please switch to a newer one or use an old snapshot of the magpie4 library")
  }
  out<-gdxAggregate(gdx = gdx,x = intake_scen,weight = 'population',to = level,absolute = FALSE,dir = dir)
  out(out,file)
}
