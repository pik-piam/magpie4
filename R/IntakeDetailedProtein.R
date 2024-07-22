#' @title IntakeDetailedProtein
#' @description Calculates food-specific per-capita protein intake from magpie results in grams.
#' @export
#' @param gdx GDX file
#' @param level Level of regional aggregation; "reg" (regional), "glo" (global), "regglo" (regional and global) or any other aggregation level defined in superAggregate
#' @param product_aggr aggregate over products or not (boolean)
#' @param dir for gridded outputs: magpie output directory which contains a mapping file (rds) for disaggregation
#' @param file a file name the output should be written to using write.magpie
#' @return Protein intake as MAgPIE object (unit: grams/cap/day)
#' @author Vartika Singh, Isabelle Weindl
#' @importFrom magclass dimSums
#' @examples
#'
#'   \dontrun{
#'     x <- IntakeDetailedProtein(gdx)
#'   }
#'

IntakeDetailedProtein <- function(gdx, file=NULL, level="reg", product_aggr=FALSE, dir="."){

  #Obtains intake calorific information at regional level, product disaggregated
  intake_scen <- IntakeDetailed(gdx, level="reg", product_aggr=FALSE)

  #Extracts information on protein from food groups
  att=readGDX(gdx=gdx,"fm_nutrition_attributes","f15_nutrition_attributes")[,getYears(intake_scen),getNames(intake_scen,dim=1)]
  intake_scen <- intake_scen / collapseNames(att[,,"kcal"]) * collapseNames(att[,,"protein"])

  if(product_aggr){intake_scen<-dimSums(intake_scen,dim=3)}

  #Aggregates to level as selected in the argument
  out<-gdxAggregate(gdx = gdx,x = intake_scen,weight = 'population',to = level,absolute = FALSE,dir = dir)

  out(out,file)

}




